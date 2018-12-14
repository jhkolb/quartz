package edu.berkeley.cs.rise.bcdsl

object Solidity {
  private val INDENTATION_STR: String = "    "

  private def writeType(ty: DataType): String =
    ty match {
      case Identity => "address"
      case Int => "int"
      case String => "bytes32"
      case Timestamp => "uint"
      case Bool => "bool"
      case Timespan => "uint"
      case Mapping(keyType, valueType) => s"mapping(${writeType(keyType)} => ${writeType(valueType)})"
    }

  private def writeField(field: Variable): String =
    INDENTATION_STR + s"${writeType(field.ty)} public ${field.name};\n"

  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
      case ValueExpression(value) => value match {
        case FieldRef(name) => builder.append(name)
        case MappingRef(mapName, key) => builder.append(s"$mapName[${writeExpression(key)}]")
        case IntConst(v) => builder.append(v)
        case StringLiteral(s) => builder.append("\"" + s + "\"")
        case Now => builder.append("now")
        case Sender => builder.append("msg.sender")
        case BoolConst(b) => builder.append(b)
        case Second => builder.append("seconds")
        case Minute => builder.append("minutes")
        case Hour => builder.append("hours")
        case Day => builder.append("days")
        case Week => builder.append("weeks")
      }

      case ArithmeticExpression(left, operator, right) =>
        left match {
          case ValueExpression(_) => builder.append(writeExpression(left))
          case _ => builder.append(s"(${writeExpression(left)})")
        }

        operator match {
          case Plus => builder.append(" + ")
          case Minus => builder.append(" - ")
          case Multiply => right match {
            case ValueExpression(Second) | ValueExpression(Minute) |
                 ValueExpression(Hour) | ValueExpression(Day) | ValueExpression(Week) => builder.append(" ")
            case _ => builder.append(" * ")
          }
          case Divide => builder.append(" / ")
        }

        right match {
          case ValueExpression(_) => builder.append(writeExpression(right))
          case _ => builder.append(s"(${writeExpression(right)})")
        }

      case LogicalExpression(left, operator, right) =>
        left match {
          case ValueExpression(_) => builder.append(writeExpression(left))
          case _ => builder.append(s"(${writeExpression(left)})")
        }
        operator match {
          case LessThan => builder.append(" < ")
          case LessThanOrEqual => builder.append(" <= ")
          case Equal => builder.append(" == ")
          case NotEqual => builder.append(" != ")
          case GreaterThanOrEqual => builder.append(" >= ")
          case GreaterThan => builder.append(" > ")
          case And => builder.append(" && ")
          case Or => builder.append(" || ")
        }
        right match {
          case ValueExpression(_) => builder.append(writeExpression(right))
          case _ => builder.append(s"(${writeExpression(right)})")
        }
    }

    builder.toString()
  }

  private def writeParameter(p: Variable): String = s"${writeType(p.ty)} ${p.name}"

  private def writeParameters(parameters: Seq[Variable]): String =
    parameters.map(writeParameter).mkString(", ")

  private def writeAssignment(assignment: Assignment): String = {
    val builder = new StringBuilder()
    assignment.leftSide match {
      case FieldRef(name) => builder.append(name)
      case MappingRef(mapName, key) => builder.append(s"$mapName[${writeExpression(key)}]")
    }
    builder.append(" = ")
    builder.append(writeExpression(assignment.rightSide))
    builder.toString()
  }

  private def writeTransition(transition: Transition, autoTransitions: Map[String, Seq[Transition]]): String = {
    val builder = new StringBuilder()

    builder.append(INDENTATION_STR)
    transition.origin match {
      case None =>
        builder.append("constructor(")
      case Some(o) =>
        builder.append(s"function ${o}_to_${transition.destination}(")
    }
    transition.parameters.foreach(params => builder.append(writeParameters(params)))
    builder.append(") public {\n")

    transition.origin.foreach { o =>
      builder.append(INDENTATION_STR * 2)
      builder.append(s"require(currentState == State.$o);\n")
    }

    // These transitions are all distinct from the current transition, but we need to interpose them
    val outgoingAutoTransitions = transition.origin.flatMap(autoTransitions.get)
    outgoingAutoTransitions.foreach(_.filter(_ != transition).zipWithIndex.foreach { case (t, idx) =>
      val g = t.guard.get
      builder.append(INDENTATION_STR * 2)
      if (idx > 0) {
        builder.append("else ")
      }
      builder.append(s"if (${writeExpression(g)}) {\n")

      if (t.destination != t.origin.get) {
        builder.append(INDENTATION_STR * 3)
        builder.append(s"currentState = State.${t.destination};\n")
      }
      t.body.foreach(_.foreach { assignment =>
        builder.append(INDENTATION_STR * 3)
        builder.append(s"${writeAssignment(assignment)};\n")
      })

      builder.append(INDENTATION_STR * 3)
      builder.append("return;\n")
      builder.append(INDENTATION_STR * 2)
      builder.append("}\n")
    })

    transition.guard.foreach { g =>
      builder.append(INDENTATION_STR * 2)
      builder.append(s"require(${writeExpression(g)});\n")
    }

    transition.authorized.foreach { authDecl =>
      val identities = authDecl.extractIdentities
      if (identities.size == 1) {
        builder.append(INDENTATION_STR * 2)
        builder.append(s"require(msg.sender == ${identities.head});\n")
      } else {
        builder.append(INDENTATION_STR * 2)
        builder.append(s"if (msg.sender == ${identities.head}) {\n")
        builder.append(INDENTATION_STR * 3)
        builder.append(s"${writeApprovalVar(transition, identities.head)} = true;\n")
        builder.append(INDENTATION_STR * 2)
        builder.append("}\n")

        identities.tail.foreach { id =>
          builder.append(INDENTATION_STR * 2)
          builder.append(s"else if (msg.sender == $id) {\n")
          builder.append(INDENTATION_STR * 3)
          builder.append(s"${writeApprovalVar(transition, id)} = true;\n")
          builder.append(INDENTATION_STR * 2)
          builder.append("}\n")
        }

        builder.append(INDENTATION_STR * 2)
        builder.append(s"require(${writeAuthClause(transition, authDecl)});\n")
      }
    }

    if (transition.origin.getOrElse("") != transition.destination) {
      // We don't need this for a self-loop
      builder.append(INDENTATION_STR * 2)
      builder.append(s"currentState = State.${transition.destination};\n")
    }

    transition.body.foreach(_.foreach { assignment =>
      builder.append(INDENTATION_STR * 2)
      builder.append(s"${writeAssignment(assignment)};\n")
    })
    builder.append(INDENTATION_STR)
    builder.append("}\n\n")

    builder.toString()
  }

  private def writeAuthorizationFields(machine: StateMachine): String = {
    val builder = new StringBuilder()

    machine.transitions.foreach(t => t.authorized.foreach { authDecl =>
      val identities = authDecl.extractIdentities
      if (identities.size > 1) {
        identities.foreach { id =>
          builder.append(INDENTATION_STR)
          builder.append(s"bool private ${writeApprovalVar(t, id)};\n")
        }
      }
    })
    builder.toString()
  }

  private def writeApprovalVar(transition: Transition, principal: String): String =
  // Validation ensures that transition must have an origin
  // Only non-initial transitions can have authorization restrictions
    s"${transition.origin.get}To${transition.destination}_${principal}Approved"

  private def writeAuthClause(transition: Transition, authDecl: AuthDecl): String =
    authDecl match {
      case AuthValue(name) => writeApprovalVar(transition, name)
      case AuthCombination(left, operator, right) =>
        val builder = new StringBuilder()
        left match {
          case AuthValue(name) => builder.append(writeApprovalVar(transition, name))
          case authCombo => builder.append(s"(${writeAuthClause(transition, authCombo)})")
        }
        operator match {
          case And => builder.append(" && ")
          case Or => builder.append(" || ")
          // This should never happen
          case _ => throw new UnsupportedOperationException(s"Operator $operator cannot be used in authorization logic")
        }
        right match {
          case AuthValue(name) => builder.append(writeApprovalVar(transition, name))
          case authCombo => builder.append(s"(${writeAuthClause(transition, authCombo)})")
        }

        builder.toString()
    }

  def writeStateMachine(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    builder.append("pragma solidity >0.4.21;\n\n")
    builder.append("contract AutoGen {\n")

    val autoTransitions = stateMachine.transitions.filter(_.auto).foldLeft(Map.empty[String, Seq[Transition]]) { (autoTrans, transition) =>
      val originState = transition.origin.get
      autoTrans + (originState -> (autoTrans.getOrElse(originState, Seq.empty[Transition]) :+ transition))
    }

    builder.append(INDENTATION_STR)
    builder.append("enum State {\n")
    builder.append(INDENTATION_STR * 2)
    builder.append(stateMachine.states.mkString(",\n" + (INDENTATION_STR * 2)))
    builder.append("\n")
    builder.append(INDENTATION_STR + "}\n")

    stateMachine.fields foreach { f => builder.append(writeField(f)) }
    builder.append(INDENTATION_STR)
    builder.append("State public currentState;\n")
    builder.append(writeAuthorizationFields(stateMachine))
    builder.append("\n")

    stateMachine.transitions foreach { t => builder.append(writeTransition(t, autoTransitions)) }

    builder.append("}")
    builder.toString
  }
}
