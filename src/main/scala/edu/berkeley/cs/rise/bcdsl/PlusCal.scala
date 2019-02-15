package edu.berkeley.cs.rise.bcdsl

object PlusCal {

  private val INDENTATION_STR = "    "
  private val BUILT_IN_CONSTANTS = Seq("MAX_INT, MIN_INT, MAX_TIMESTEP")
  private val CONTRACT_BALANCE_VAR = "__contractBalance"
  private val CALL_DEPTH_VAR = "__contractCallDepth"
  private val CURRENT_TIME_VAR = "__currentTime"
  private var labelCounter = 0

  private def writeDomain(ty: DataType): String = ty match {
    case Identity => "IDENTITIES \\ {ZERO}"
    case Int => "MIN_INT..MAX_INT"
    case Bool => "{ TRUE, FALSE }"
    case Timestamp => "0..MAX_INT"
    case Timespan => "0..MAX_INT"
    case String => throw new NotImplementedError("Strings have infinite domain") // TODO
    case Mapping(keyType, valueType) => s"[ x \\in ${writeDomain(keyType)} -> ${writeDomain(valueType)} ]"
  }

  private def writeZeroElement(ty: DataType): String = ty match {
    case Identity => "ZERO_IDENT"
    case Int => "0"
    case String => "\"\""
    case Timestamp => "0"
    case Bool => "FALSE"
    case Timespan => "0"
    case Mapping(keyType, valueType) => s"[ x \\in ${writeDomain(keyType)} |-> ${writeZeroElement(valueType)} ]"
  }

  private def writeField(field: Variable): String = s"${field.name} = ${writeZeroElement(field.ty)}"

  private def writeTransitionApprovalVar(transition: Transition, principal: String): String =
  // Only non-initial transitions can have authorization clause
    s"${transition.name}_${principal}_approved"

  private def writeTransitionApprovalFields(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    stateMachine.transitions.foreach(t => t.authorized.map(_.extractIdentities).filter(_.size > 1).foreach(_.foreach { id =>
      builder.append(",\n")
      builder.append(INDENTATION_STR)
      builder.append(s"${writeTransitionApprovalVar(t, id)} = FALSE")
    }))

    builder.toString()
  }

  private def writeAuthClause(transition: Transition, decl: AuthDecl): String = {
    val builder = new StringBuilder()
    decl match {
      case AuthValue(name) => builder.append(writeTransitionApprovalVar(transition, name))
      case AuthCombination(left, op, right) =>
        left match {
          case AuthValue(name) => builder.append(writeTransitionApprovalVar(transition, name))
          case authCombination => builder.append(s"(${writeAuthClause(transition, authCombination)})")
        }

        op match {
          case And => builder.append(" /\\ ")
          case Or => builder.append(" \\/ ")
          // This should never happen
          case _ => throw new UnsupportedOperationException(s"Operator $op cannot be used in authorization logic")
        }

        right match {
          case AuthValue(name) => builder.append(writeTransitionApprovalVar(transition, name))
          case authCombination => builder.append(s"(${writeAuthClause(transition, authCombination)})")
        }
    }

    builder.toString()
  }

  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
      case ValueExpression(value) => value match {
        case VarRef(name) => builder.append(name)
        case MappingRef(mapName, key) => builder.append(s"$mapName[${writeExpression(key)}]")
        case IntConst(v) => builder.append(v)
        case StringLiteral(s) => builder.append("\"" + s + "\"")
        case Now => builder.append(s"$CURRENT_TIME_VAR")
        case Sender => builder.append("sender")
        case BoolConst(b) => builder.append(b.toString.toUpperCase)
        case Second => builder.append("1")
        case Minute => builder.append("60")
        case Hour => builder.append("3600")
        case Day => builder.append("86400")
        case Week => builder.append("604800")
      }

      case ArithmeticExpression(left, op, right) =>
        left match {
          case ValueExpression(_) => builder.append(writeExpression(left))
          case _ => builder.append(s"(${writeExpression(left)})")
        }

        op match {
          case Plus => builder.append(" + ")
          case Minus => builder.append(" - ")
          case Multiply => builder.append(" * ")
          case Divide => builder.append(" / ")
        }

        right match {
          case ValueExpression(_) => builder.append(writeExpression(right))
          case _ => builder.append(s"(${writeExpression(right)})")
        }

      case LogicalExpression(left, op, right) =>
        left match {
          case ValueExpression(_) => builder.append(writeExpression(left))
          case _ => builder.append(s"(${writeExpression(left)})")
        }

        op match {
          case LessThan => builder.append(" < ")
          case LessThanOrEqual => builder.append(" <= ")
          case Equal => builder.append(" = ")
          case NotEqual => builder.append(" /= ")
          case GreaterThanOrEqual => builder.append(" >= ")
          case GreaterThan => builder.append(" > ")
          case And => builder.append(" /\\ ")
          case Or => builder.append(" \\/ ")
        }

        right match {
          case ValueExpression(_) => builder.append(writeExpression(right))
          case _ => builder.append(s"(${writeExpression(right)})")
        }
    }

    builder.toString()
  }

  private def writeStatement(statement: Statement): String = statement match {
    case Assignment(left, right) =>
      val leftStr = left match {
        case VarRef(name) => name
        case MappingRef(mapName, key) => s"$mapName[${writeExpression(key)}]"
      }
      s"$leftStr := ${writeExpression(right)};"

    case Send(destination, amount) => s"call sendTokens(${writeExpression(destination)}, ${writeExpression(amount)});\n${nextLabel()}"
  }

  private def nextLabel(): String = {
    val currentLabelIndex = labelCounter
    labelCounter += 1
    s"L$currentLabelIndex:"
  }

  private def writeTransition(transition: Transition): String = {
    val builder = new StringBuilder()
    builder.append(s"procedure ${transition.name}(")
    transition.parameters.foreach { params =>
      builder.append(params.map(_.name).mkString(", "))
    }
    builder.append(")")
    builder.append(s" begin ${transition.name}:\n")


    transition.origin.foreach { o =>
      builder.append(INDENTATION_STR)
      builder.append(s"if currentState /= ${o.toUpperCase()} then\n")
      builder.append(INDENTATION_STR * 2 + "return;\n")
      builder.append(INDENTATION_STR + "end if;\n")
      builder.append(nextLabel() + "\n")
    }

    transition.guard.foreach { g =>
      builder.append(INDENTATION_STR)
      builder.append(s"if ~(${writeExpression(g)}) then\n")
      builder.append(INDENTATION_STR * 2)
      builder.append("return;\n")
      builder.append(INDENTATION_STR)
      builder.append("end if;\n")
      builder.append(nextLabel() + "\n")
    }

    transition.authorized.foreach { authDecl =>
      val identities = authDecl.extractIdentities.map(_.toUpperCase)
      if (identities.size == 1) {
        builder.append(INDENTATION_STR)
        builder.append(s"if sender /= ${identities.head} then\n")
        builder.append(INDENTATION_STR * 2)
        builder.append("return;\n")
        builder.append(INDENTATION_STR)
        builder.append("end if;\n")
        builder.append(nextLabel() + "\n")
      } else {
        builder.append(INDENTATION_STR)
        builder.append(s"if sender = ${identities.head} then\n")
        builder.append(INDENTATION_STR * 2)
        builder.append(s"${writeTransitionApprovalVar(transition, identities.head)} := TRUE;\n")

        identities.tail.foreach { id =>
          builder.append(INDENTATION_STR)
          builder.append(s"elsif sender = $id then\n")
          builder.append(INDENTATION_STR * 2)
          builder.append(s"${writeTransitionApprovalVar(transition, id)} := TRUE;\n")
        }
        builder.append(INDENTATION_STR)
        builder.append("end if;\n")
        builder.append(nextLabel() + "\n")

        builder.append(INDENTATION_STR)
        builder.append(s"if ~(${writeAuthClause(transition, authDecl)}) then\n")
        builder.append(INDENTATION_STR * 2)
        builder.append("return;\n")
        builder.append(INDENTATION_STR)
        builder.append("end if;\n")
        builder.append(nextLabel() + "\n")
      }
    }

    if (transition.origin.getOrElse("") != transition.destination) {
      builder.append(INDENTATION_STR)
      builder.append(s"currentState := ${transition.destination.toUpperCase};\n")
    }
    transition.body.foreach(_.foreach { statement =>
      builder.append(INDENTATION_STR)
      builder.append(s"${writeStatement(statement)}\n")
    })

    builder.append(INDENTATION_STR)
    builder.append("return;\n")
    builder.append("end procedure;\n\n")

    builder.toString()
  }

  private def writeTransitionArgumentSelection(transition: Transition): String =
  // Add "_arg" suffix to avoid name collisions in the TLA+ that gets produced from this PlusCal
    "with " + transition.parameters.get.map(p => s"${p.name + "_arg"} \\in ${writeDomain(p.ty)}").mkString(", ") + " do"

  // TODO code cleanup
  private def writeInvocationLoop(transitions: Seq[Transition]): String = {
    val builder = new StringBuilder()
    builder.append("procedure invokeContract(sender) begin InvokeContract:\n")
    builder.append(INDENTATION_STR + s"$CALL_DEPTH_VAR := $CALL_DEPTH_VAR + 1;\n")
    builder.append(INDENTATION_STR + "with __timeDelta \\in 1..MAX_TIMESTEP do\n")
    builder.append(INDENTATION_STR * 2 + s"$CURRENT_TIME_VAR := $CURRENT_TIME_VAR + __timeDelta;\n")
    builder.append(INDENTATION_STR + "end with;\n")

    builder.append("\nMethodCall:\n")
    if (transitions.length == 1) {
      builder.append(INDENTATION_STR + writeTransitionArgumentSelection(transitions.head) + "\n")
      builder.append(INDENTATION_STR * 2)
      builder.append(s"call ${transitions.head.name}(")
      builder.append(transitions.head.parameters.get.map(_.name))
      builder.append(");\n")
      builder.append(INDENTATION_STR + "end with;")
    } else {
      builder.append(INDENTATION_STR + "either\n")
      transitions.zipWithIndex.foreach { case (transition, idx) =>
        builder.append(INDENTATION_STR * 2 + writeTransitionArgumentSelection(transition) + "\n")
        builder.append(INDENTATION_STR * 3 + s"call ${transition.name}(")
        // Again, add "_arg" suffix to avoid name collisions in TLA+
        builder.append(transition.parameters.get.map(_.name + "_arg").mkString(", "))
        builder.append(");\n")
        builder.append(INDENTATION_STR * 2 + "end with;\n")

        if (idx < transitions.length - 1) {
          builder.append(INDENTATION_STR + "or\n")
        }
      }
      builder.append(INDENTATION_STR + "end either;\n")
    }

    builder.append("end procedure;\n")
    builder.toString()
  }

  private def writeSendFunction(): String = {
    val builder = new StringBuilder()
    builder.append("procedure sendTokens(sender, amount) begin SendTokens:\n")
    builder.append(INDENTATION_STR + s"$CONTRACT_BALANCE_VAR := $CONTRACT_BALANCE_VAR - amount;\n")
    builder.append("SendInvocation:\n")
    builder.append(INDENTATION_STR + "either\n")
    builder.append(INDENTATION_STR * 2 + "call invokeContract(sender);\n")
    builder.append(INDENTATION_STR + "or\n")
    builder.append(INDENTATION_STR * 2 + "skip;\n")
    builder.append(INDENTATION_STR + "end either;\n")
    builder.append("end procedure;\n")
    builder.toString()
  }

  def writeSpecification(specification: Specification): String = specification match {
    case Specification(name, stateMachine, _) =>
      val initialState = stateMachine.transitions.filter(_.origin.isEmpty).head.destination

      val builder = new StringBuilder()
      builder.append(s"-----MODULE $name-----\n")
      builder.append("EXTENDS Integers, Sequences, TLC\n")

      val stateNames = stateMachine.states.map(_.toUpperCase)
      val identityNames = stateMachine.fields.filter(_.ty == Identity).map(_.name.toUpperCase()) :+ "ZERO"
      builder.append(s"CONSTANTS ${BUILT_IN_CONSTANTS.mkString(", ")}\n")
      builder.append(s"CONSTANTS ${stateNames.mkString(", ")}\n")
      builder.append(s"CONSTANTS ${identityNames.mkString(", ")}\n")
      builder.append(s"STATES == { ${stateMachine.states.map(_.toUpperCase).mkString(", ")} }\n")
      builder.append(s"IDENTITIES == { ${identityNames.mkString(", ")} }\n\n")

      builder.append(s"(* --fair algorithm $name\n")

      builder.append(s"variables currentState = ${initialState.toUpperCase},\n")
      builder.append(INDENTATION_STR + s"$CURRENT_TIME_VAR = 0,\n")
      builder.append(INDENTATION_STR + s"$CALL_DEPTH_VAR = 0,\n")
      builder.append(INDENTATION_STR + s"$CONTRACT_BALANCE_VAR = 0")
      builder.append(writeTransitionApprovalFields(stateMachine))

      if (stateMachine.fields.nonEmpty) {
        builder.append(",\n" + INDENTATION_STR)
        builder.append(stateMachine.fields.map(writeField).mkString(",\n" + INDENTATION_STR))
      }
      builder.append(";\n\n")

      // Add synthetic "sender" parameter to all transitions
      val augmentedTransitions = stateMachine.transitions.map { case Transition(tName, origin, destination, parameters, authorized, auto, guard, body) =>
        val newParameters = Variable("sender", Identity) +: parameters.getOrElse(Seq.empty[Variable])
        Transition(tName, origin, destination, Some(newParameters), authorized, auto, guard, body)
      }

      val initialTransition = augmentedTransitions.filter(_.origin.isEmpty).head
      val standardTransitions = augmentedTransitions.filter(_.origin.isDefined)
      builder.append(writeTransition(initialTransition))
      standardTransitions.foreach(t => builder.append(writeTransition(t)))

      builder.append(writeInvocationLoop(standardTransitions))
      builder.append("\n")
      builder.append(writeSendFunction())

      // Invoke procedure corresponding to initial transition
      builder.append("\nbegin Main:\n")
      // Add the usual "_arg" suffix to prevent name collisions in TLA+
      builder.append(INDENTATION_STR + "with ")
      builder.append(initialTransition.parameters.get.map(p => s"${p.name + "_arg"} \\in ${writeDomain(p.ty)}").mkString(", "))
      builder.append(" do\n")
      builder.append(INDENTATION_STR * 2)
      builder.append(s"call ${initialTransition.name}(${initialTransition.parameters.get.map(_.name + "_arg").mkString(", ")});\n")
      builder.append(INDENTATION_STR + "end with;\n")

      builder.append("\nLoop:\n")
      builder.append(INDENTATION_STR + s"with senderArg \\in ${writeDomain(Identity)} do\n")
      builder.append(INDENTATION_STR * 2 + "call invokeContract(senderArg);\n")
      builder.append(INDENTATION_STR + "end with;\n")
      builder.append(nextLabel() + "\n")
      builder.append(INDENTATION_STR + "either\n")
      builder.append(INDENTATION_STR * 2 + "goto Loop;\n")
      builder.append(INDENTATION_STR + "or\n")
      builder.append(INDENTATION_STR * 2 + "skip;\n")
      builder.append(INDENTATION_STR + "end either;\n")

      builder.append("end algorithm; *)\n")
      builder.append("========")
      builder.toString()
  }
}
