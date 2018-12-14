package edu.berkeley.cs.rise.bcdsl

object PlusCal {

  private val INDENTATION_STR = "    "

  private def writeDomain(ty: DataType): String = ty match {
    case Identity => "IDENTITIES"
    case Int => "MIN_INT..MAX_INT"
    case Bool => "{ TRUE, FALSE }"
    case Timestamp => "0..MAX_INT"
    case Timespan => "0..MAX_INT"
    case String => throw new NotImplementedError("Strings have infinite domain") // TODO
    case Mapping(keyType, valueType) => s"[ x \\in ${writeDomain(keyType)} -> ${writeDomain(valueType)} ]"
  }

  private def writeZeroElement(ty: DataType): String = ty match {
    case Identity => "ZERO"
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
    s"${transition.origin.get}To${transition.destination}Approved"

  private def writeTransitionApprovalFields(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    stateMachine.transitions.foreach(t => t.authorized.map(_.extractIdentities).filter(_.size > 1).foreach(_.foreach { id =>
      builder.append(INDENTATION_STR)
      builder.append(s"${writeTransitionApprovalVar(t, id)} = FALSE,\n")
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
        case FieldRef(name) => builder.append(name)
        case MappingRef(mapName, key) => builder.append(s"$mapName[$key]")
        case IntConst(v) => builder.append(v)
        case StringLiteral(s) => builder.append("\"" + s + "\"")
        case Now => builder.append("currentTime")
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

  private def writeAssignment(assignment: Assignment): String = {
    val left = assignment.leftSide match {
      case FieldRef(name) => name
      case MappingRef(mapName, key) => s"$mapName[${writeExpression(key)}]"
    }

    s"$left := ${writeExpression(assignment.rightSide)}"
  }

  private def writeTransition(transition: Transition): String = {
    val builder = new StringBuilder()
    builder.append(s"procedure ${transition.origin.get}_to_${transition.destination}(")
    transition.parameters.foreach { params =>
      builder.append(params.map(_.name).mkString(", "))
    }
    builder.append(")")
    builder.append(s" begin ${transition.origin.get}_to_${transition.destination}:\n")


    transition.guard.foreach { g =>
      builder.append(INDENTATION_STR)
      builder.append(s"if ~(${writeExpression(g)}) then\n")
      builder.append(INDENTATION_STR * 2)
      builder.append("return;\n")
      builder.append(INDENTATION_STR)
      builder.append("end if;\n")
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
      } else {
        builder.append(INDENTATION_STR)
        builder.append(s"if sender = ${identities.head} then\n")
        builder.append(INDENTATION_STR * 2)
        builder.append(s"${writeTransitionApprovalVar(transition, identities.head)} := TRUE;\n")

        identities.tail.foreach { id =>
          builder.append(INDENTATION_STR)
          builder.append(s"elsif sender == $id then\n")
          builder.append(INDENTATION_STR * 2)
          builder.append(s"${writeTransitionApprovalVar(transition, id)} := TRUE;\n")
        }
        builder.append(INDENTATION_STR)
        builder.append("end if;\n")

        builder.append(INDENTATION_STR)
        builder.append(s"if ~(${writeAuthClause(transition, authDecl)}) then\n")
        builder.append(INDENTATION_STR * 2)
        builder.append("return;\n")
        builder.append(INDENTATION_STR)
        builder.append("end if;\n")
      }
    }

    if (transition.origin.get != transition.destination) {
      builder.append(INDENTATION_STR)
      builder.append(s"currentState := ${transition.destination.toUpperCase};\n")
    }
    transition.body.foreach(_.foreach { assignment =>
      builder.append(INDENTATION_STR)
      builder.append(s"${writeAssignment(assignment)};\n")
    })

    builder.append(INDENTATION_STR)
    builder.append("return;\n")
    builder.append("end procedure;\n\n")

    builder.toString()
  }

  def writeStateMachine(stateMachine: StateMachine): String = {
    val initialState = stateMachine.transitions.filter(_.origin.isEmpty).head.destination

    val builder = new StringBuilder()
    builder.append("-----MODULE Autogen-----\n")
    builder.append("EXTENDS Integers, Sequences, TLC\n")

    val stateNames = stateMachine.states.map(_.toUpperCase)
    val identityNames = stateMachine.fields.filter(_.ty == Identity).map(_.name.toUpperCase()) ++ Seq("OTHER", "ZERO")
    builder.append(s"CONSTANTS ${stateNames.mkString(", ")}\n")
    builder.append(s"CONSTANTS ${identityNames.mkString(", ")}\n")
    builder.append(s"STATES == { ${stateMachine.states.map(_.toUpperCase).mkString(", ")} }\n")
    builder.append(s"IDENTITIES == { ${identityNames.mkString(", ")} }\n\n")

    builder.append("(* --fair algorithm Autogen\n")

    builder.append(s"variables currentState = ${initialState.toUpperCase},\n")
    builder.append(INDENTATION_STR + "currentTime = 0")
    builder.append(writeTransitionApprovalFields(stateMachine))

    if (stateMachine.fields.nonEmpty) {
      builder.append(",\n" + INDENTATION_STR)
      builder.append(stateMachine.fields.map(writeField).mkString(",\n" + INDENTATION_STR))
    }
    builder.append(";\n\n")

    // Add synthetic "sender" parameter to all transitions
    val augmentedTransitions = stateMachine.transitions.map { case Transition(origin, destination, parameters, authorized, auto, guard, body) =>
      val newParameters = Variable("sender", Identity) +: parameters.getOrElse(Seq.empty[Variable])
      Transition(origin, destination, Some(newParameters), authorized, auto, guard, body)
    }
    augmentedTransitions.filter(_.origin.isDefined).foreach(t => builder.append(writeTransition(t)))

    builder.append("end algorithm; *)\n")
    builder.toString()
  }
}
