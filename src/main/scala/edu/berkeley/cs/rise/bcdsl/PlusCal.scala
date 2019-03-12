package edu.berkeley.cs.rise.bcdsl

object PlusCal {

  private val INDENTATION_STR = "    "
  private val BUILT_IN_CONSTANTS = Seq("MAX_INT, MIN_INT, MAX_TIMESTEP", "MAX_ELAPSED_TIME", "MAX_CALL_DEPTH")
  private val CONTRACT_BALANCE_VAR = "balance"
  private[bcdsl] val CALL_DEPTH_VAR = "__contractCallDepth"
  private[bcdsl] val CURRENT_TIME_VAR = "__currentTime"
  private val CURRENT_STATE_VAR = "__currentState"

  private var labelCounter = 0
  private var indentationLevel = 0

  private val RESERVED_NAME_TRANSLATIONS: Map[String, String] = Map[String, String](
    "balance" -> "balance",
    "sender" -> "sender",
    "now" -> "__currentTime",
  )

  private def appendLine(builder: StringBuilder, line: String): Unit =
    builder.append(s"${INDENTATION_STR * indentationLevel}$line\n")

  private def writeLine(line: String): String = s"${INDENTATION_STR * indentationLevel}$line\n"

  private def writeDomain(ty: DataType): String = ty match {
    case Identity => "IDENTITIES \\ {ZERO_IDENT}"
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
      appendLine(builder, s"variable ${writeTransitionApprovalVar(t, id)} = FALSE")
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
        case VarRef(name) => builder.append(RESERVED_NAME_TRANSLATIONS.getOrElse(name, name))
        case MappingRef(mapName, key) => builder.append(s"$mapName[${writeExpression(key)}]")
        case IntConst(v) => builder.append(v)
        case StringLiteral(s) => builder.append("\"" + s + "\"")
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

  private def writeAssignable(assignable: Assignable): String = assignable match {
    case VarRef(name) => name
    case MappingRef(mapName, key) => s"$mapName[${writeExpression(key)}]"
  }

  private def writeStatement(statement: Statement): String = statement match {
    case Assignment(left, right) => writeLine(s"${writeAssignable(left)} := ${writeExpression(right)};")
    case Send(destination, amount, source) => source match {
      case None => writeLine(s"call sendTokens(${writeExpression(destination)}, ${writeExpression(amount)});") + nextLabel() + "\n"
      case Some(s) =>
        val builder = new StringBuilder()
        appendLine(builder, s"__temporary := ${writeExpression(amount)};")
        appendLine(builder, s"${writeAssignable(s)} := ${writeAssignable(s)} - __temporary;")
        appendLine(builder, s"call sendTokens(${writeExpression(destination)}, __temporary);")
        builder.append(nextLabel() + "\n")
        builder.toString()
    }
  }

  private def nextLabel(): String = {
    val currentLabelIndex = labelCounter
    labelCounter += 1
    s"L$currentLabelIndex:"
  }

  private def writeTransition(transition: Transition): String = {
    val builder = new StringBuilder()
    val effectiveParameters = Variable("sender", Identity) +: transition.parameters.getOrElse(Seq.empty[Variable])
    val paramsRepr = effectiveParameters.map(_.name).mkString(", ")
    appendLine(builder, s"procedure ${transition.name}($paramsRepr) begin ${transition.name}:")
    indentationLevel += 1

    transition.origin.foreach { o =>
      appendLine(builder, s"if $CURRENT_STATE_VAR /= ${o.toUpperCase} then")
      indentationLevel += 1
      appendLine(builder,"return;")
      indentationLevel -= 1
      appendLine(builder, "end if;")
      builder.append(nextLabel() + "\n")
    }

    transition.guard.foreach { g =>
      appendLine(builder, s"if ~(${writeExpression(g)}) then")
      indentationLevel += 1
      appendLine(builder, "return;")
      indentationLevel -= 1
      appendLine(builder, "end if;")
      builder.append(nextLabel() + "\n")
    }

    transition.authorized.foreach { authDecl =>
      val identities = authDecl.extractIdentities.map(_.toUpperCase)
      if (identities.size == 1) {
        appendLine(builder, s"if sender /= ${identities.head} then")
        indentationLevel += 1
        builder.append("return;")
        indentationLevel -= 1
        builder.append("end if;")
        builder.append(nextLabel() + "\n")
      } else {
        appendLine(builder, s"if sender = ${identities.head} then")
        indentationLevel += 1
        appendLine(builder, s"${writeTransitionApprovalVar(transition, identities.head)} := TRUE;")
        indentationLevel -= 1

        identities.tail.foreach { id =>
          appendLine(builder, s"elsif sender = $id then")
          indentationLevel += 1
          appendLine(builder, s"${writeTransitionApprovalVar(transition, id)} := TRUE;")
          indentationLevel -= 1
        }
        appendLine(builder, "end if;")
        builder.append(nextLabel() + "\n")

        appendLine(builder, s"if ~(${writeAuthClause(transition, authDecl)}) then")
        indentationLevel += 1
        appendLine(builder, "return;")
        indentationLevel -= 1
        appendLine(builder, "end if;")
        builder.append(nextLabel() + "\n")
      }
    }

    if (transition.origin.getOrElse("") != transition.destination) {
      appendLine(builder, s"$CURRENT_STATE_VAR := ${transition.destination.toUpperCase};")
    }
    transition.body.foreach(_.foreach { statement =>
      // Statements handle their own indentation
      builder.append(s"${writeStatement(statement)}")
    })

    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.append("\n")
    builder.toString()
  }

  private def writeTransitionArgumentSelection(parameters: Seq[Variable]): String =
  // Add "_arg" suffix to avoid name collisions in the TLA+ that gets produced from this PlusCal
    "with " + parameters.map(p => s"${p.name + "_arg"} \\in ${writeDomain(p.ty)}").mkString(", ") + " do"

  // TODO code cleanup
  private def writeInvocationLoop(transitions: Seq[Transition]): String = {
    val builder = new StringBuilder()
    appendLine(builder, "procedure invokeContract(sender) begin InvokeContract:")
    indentationLevel += 1
    appendLine(builder, s"$CALL_DEPTH_VAR := $CALL_DEPTH_VAR + 1;")
    appendLine(builder, "with __timeDelta \\in 1..MAX_TIMESTEP do")
    indentationLevel += 1
    appendLine(builder, s"$CURRENT_TIME_VAR := $CURRENT_TIME_VAR + __timeDelta;")
    indentationLevel -= 1
    appendLine(builder, "end with;")
    builder.append("MethodCall:\n")
    if (transitions.length == 1) {
      val t = transitions.head
      t.parameters.foreach(params => {
        appendLine(builder, writeTransitionArgumentSelection(params))
        indentationLevel += 1
      })

      val paramsRepr = t.parameters.fold("")(p => p.map(_.name + "_ arg").mkString(" ,"))
      appendLine(builder,s"call ${t.name}($paramsRepr);")
      if (t.parameters.isDefined) {
        indentationLevel -= 1
        appendLine(builder, "end with")
      }
    } else {
      appendLine(builder, "either")
      indentationLevel += 1
      transitions.zipWithIndex.foreach { case (transition, idx) =>
        transition.parameters.foreach { params =>
          appendLine(builder, writeTransitionArgumentSelection(params))
          indentationLevel += 1
        }

        // Again, add "_arg" suffix to avoid name collisions in TLA+
        val paramsRepr = ("sender" +: transition.parameters.getOrElse(Seq.empty[Variable]).map(_.name + "_arg")).mkString(", ")
        appendLine(builder, s"call ${transition.name}($paramsRepr);")
        if (transition.parameters.isDefined) {
          indentationLevel -= 1
          appendLine(builder, "end with;")
        }

        indentationLevel -= 1
        if (idx < transitions.length - 1) {
          appendLine(builder, "or")
          indentationLevel += 1
        } else {
          appendLine(builder, "end either;")
        }
      }
    }

    builder.append(nextLabel() + "\n")
    appendLine(builder, s"$CALL_DEPTH_VAR := $CALL_DEPTH_VAR - 1;")
    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.toString()
  }

  private def writeSendFunction(): String = {
    val builder = new StringBuilder()
    appendLine(builder, "procedure sendTokens(recipient, amount) begin SendTokens:")
    indentationLevel += 1
    appendLine(builder, s"$CONTRACT_BALANCE_VAR := $CONTRACT_BALANCE_VAR - amount;")
    builder.append("SendInvocation:\n")
    appendLine(builder, "either")
    indentationLevel += 1
    appendLine(builder, "call invokeContract(recipient);")
    appendLine(builder, "goto SendInvocation;")
    indentationLevel -= 1
    appendLine(builder, "or")
    indentationLevel += 1
    appendLine(builder, "skip;")
    indentationLevel -= 1
    appendLine(builder, "end either;")
    builder.append(nextLabel() + "\n")
    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.toString()
  }

  def writeSpecification(specification: Specification): String = specification match {
    case Specification(name, stateMachine, _) =>
      val initialState = stateMachine.transitions.filter(_.origin.isEmpty).head.destination

      val builder = new StringBuilder()
      appendLine(builder, s"-----MODULE $name-----")
      appendLine(builder, "EXTENDS Integers, Sequences, TLC")

      val stateNames = stateMachine.states.map(_.toUpperCase)
      val identityNames = stateMachine.fields.filter(_.ty == Identity).map(_.name.toUpperCase) :+ "ZERO_IDENT"
      appendLine(builder, s"CONSTANTS ${BUILT_IN_CONSTANTS.mkString(", ")}")
      appendLine(builder, s"CONSTANTS ${stateNames.mkString(", ")}")
      appendLine(builder, s"CONSTANTS ${identityNames.mkString(", ")}")
      appendLine(builder, s"STATES == { ${stateMachine.states.map(_.toUpperCase).mkString(", ")} }")
      appendLine(builder, s"IDENTITIES == { ${identityNames.mkString(", ")} }")
      builder.append("\n")

      appendLine(builder, s"(* --fair algorithm $name")
      appendLine(builder, s"variables $CURRENT_STATE_VAR = ${initialState.toUpperCase};")
      indentationLevel += 1
      appendLine(builder, s"$CURRENT_TIME_VAR = 0;")
      appendLine(builder, s"$CALL_DEPTH_VAR = 0;")
      appendLine(builder, s"$CONTRACT_BALANCE_VAR = 0;")
      builder.append(writeTransitionApprovalFields(stateMachine))

      stateMachine.fields.foreach { field =>
        appendLine(builder, s"${writeField(field)};")
      }
      builder.append("\n")

      val initialTransition = stateMachine.transitions.filter(_.origin.isEmpty).head
      val standardTransitions = stateMachine.transitions.filter(_.origin.isDefined)
      builder.append(writeTransition(initialTransition))
      standardTransitions.foreach(t => builder.append(writeTransition(t)))

      builder.append(writeInvocationLoop(standardTransitions))
      builder.append("\n")
      builder.append(writeSendFunction())
      builder.append("\n")

      // Invoke procedure corresponding to initial transition
      builder.append("begin Main:\n")
      // Add the usual "_arg" suffix to prevent name collisions in TLA+
      val initialParams = Variable("sender", Identity) +: initialTransition.parameters.getOrElse(Seq.empty[Variable])
      val initialParamsChoice = initialParams.map(p => s"${p.name}_arg \\in ${writeDomain(p.ty)}").mkString(", ")
      appendLine(builder, s"with $initialParamsChoice do")
      indentationLevel += 1
      appendLine(builder, s"call ${initialTransition.name}(${initialParams.map(_.name + "_arg").mkString(", ")});")
      indentationLevel -= 1
      appendLine(builder, "end with;")
      builder.append("\n")

      builder.append("Loop:\n")
      appendLine(builder, s"with sender_arg \\in ${writeDomain(Identity)} do")
      indentationLevel += 1
      appendLine(builder, "call invokeContract(sender_arg);")
      indentationLevel -= 1
      appendLine(builder, "end with;")
      builder.append(nextLabel() + "\n")
      appendLine(builder, "goto Loop;")

      indentationLevel -= 1
      appendLine(builder, "end algorithm; *)")
      appendLine(builder, "========")
      builder.toString()
  }
}
