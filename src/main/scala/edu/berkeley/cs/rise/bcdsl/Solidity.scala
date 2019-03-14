package edu.berkeley.cs.rise.bcdsl

object Solidity {
  private val INDENTATION_STR: String = "    "
  private val CURRENT_STATE_VAR: String = "__currentState"
  private val RESERVED_NAME_TRANSLATIONS: Map[String, String] = Map[String, String](
    "balance" -> "balance",
    "now" -> "now",
    "sender" -> "msg.sender",
  )

  private var indentationLevel: Integer = 0

  private def appendLine(builder: StringBuilder, line: String): Unit =
    builder.append(s"${INDENTATION_STR * indentationLevel}$line\n")

  private def writeLine(line: String): String = s"${INDENTATION_STR * indentationLevel}$line\n"

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
    s"${writeType(field.ty)} public ${field.name}"

  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
      case ValueExpression(value) => value match {
        case VarRef(name) => builder.append(RESERVED_NAME_TRANSLATIONS.getOrElse(name, name))
        case MappingRef(mapName, key) => builder.append(s"$mapName[${writeExpression(key)}]")
        case IntConst(v) => builder.append(v)
        case StringLiteral(s) => builder.append("\"" + s + "\"")
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

  private def writeAssignable(assignable: Assignable): String = assignable match {
    case VarRef(name) => name
    case MappingRef(mapName, key) => s"$mapName[${writeExpression(key)}]"
  }

  private def writeStatement(statement: Statement): String = statement match {
    case Assignment(left, right) => writeLine(s"${writeAssignable(left)} = ${writeExpression(right)};")
    case Send(destination, amount, source) =>
      val destStr = destination match {
        case ValueExpression(_) => writeExpression(destination)
        case _ => s"(${writeExpression(destination)};)"
      }
      source match {
        // TODO we just convert to uint as needed for now, but this assumes amount >= 0
        case None => writeLine(s"$destStr.transfer(uint(${writeExpression(amount)}));")
        case Some(s) =>
          val builder = new StringBuilder()
          appendLine(builder, s"int __temporary = ${writeExpression(amount)};")
          appendLine(builder, s"${writeAssignable(s)} = ${writeAssignable(s)} - __temporary;")
          appendLine(builder, s"$destStr.transfer(uint(__temporary));")
          builder.toString()
      }
  }

  private def writeTransition(transition: Transition, autoTransitions: Map[String, Seq[Transition]]): String = {
    val builder = new StringBuilder()

    val paramsRepr = transition.parameters.fold("")(writeParameters)
    if (transition.origin.isDefined) {
      appendLine(builder, s"function ${transition.name} ($paramsRepr) public {")
    } else {
      appendLine(builder, s"constructor($paramsRepr) public {")
    }
    indentationLevel += 1

    transition.origin.foreach { o =>
      appendLine(builder, s"require($CURRENT_STATE_VAR == State.$o);")
    }

    // These transitions are all distinct from the current transition, but we need to interpose them
    val outgoingAutoTransitions = transition.origin.flatMap(autoTransitions.get)
    outgoingAutoTransitions.foreach(_.filter(_ != transition).zipWithIndex.foreach { case (t, idx) =>
      val g = t.guard.get // Auto transitions must have a guard
      if (idx == 0) {
        appendLine(builder, s"if (${writeExpression(g)}) {")
      } else {
        appendLine(builder, s"else if (${writeExpression(g)}) {")
      }
      indentationLevel += 1

      if (t.destination != t.origin.get) {
        appendLine(builder, s"$CURRENT_STATE_VAR = State.${t.destination};")
      }
      t.body.foreach(_.foreach(s => builder.append(writeStatement(s))))

      appendLine(builder, "return;")
      indentationLevel -= 1
      appendLine(builder, "}")
    })

    transition.guard.foreach { g =>
      appendLine(builder, s"require(${writeExpression(g)});")
    }

    transition.authorized.foreach { authDecl =>
      val identities = authDecl.extractIdentities
      if (identities.size == 1) {
        appendLine(builder, s"require(msg.sender == ${identities.head});")
      } else {
        appendLine(builder, s"if (msg.sender == ${identities.head}) {")
        indentationLevel += 1
        appendLine(builder, s"${writeApprovalVarRef(transition, identities.head)} = true;")
        indentationLevel -= 1
        appendLine(builder, "}")

        identities.tail.foreach { id =>
          appendLine(builder, s"else if (msg.sender == $id) {")
          indentationLevel += 1
          appendLine(builder, s"${writeApprovalVarRef(transition, id)} = true;")
          indentationLevel -= 1
          appendLine(builder, "}")
        }

        appendLine(builder, s"require(${writeAuthClause(transition, authDecl)});")
      }
    }

    if (transition.origin.getOrElse("") != transition.destination) {
      // We don't need this for a self-loop
      appendLine(builder, s"$CURRENT_STATE_VAR = State.${transition.destination};")
    }

    transition.body.foreach(_.foreach(s => builder.append(writeStatement(s))))
    indentationLevel -= 1
    appendLine(builder, "}")
    builder.append("\n")
    builder.toString()
  }

  private def writeAuthorizationFields(machine: StateMachine): String = {
    val builder = new StringBuilder()

    machine.transitions.foreach(t => t.authorized.foreach { authDecl =>
      val identities = authDecl.extractIdentities
      if (identities.size > 1) {
        identities.foreach { id =>
          appendLine(builder, s"${writeApprovalVarType(t)} private ${writeApprovalVarName(t, id)};")
        }
      }
    })
    builder.toString()
  }

  private def writeApprovalVarName(transition: Transition, principal: String): String =
    // Validation ensures that transition must have an origin
    // Only non-initial transitions can have authorization restrictions
    s"__${transition.name}_${principal}Approved"

  private def writeApprovalVarRef(transition: Transition, principal: String): String = transition.parameters match {
    case None => writeApprovalVarName(transition, principal)
    case Some(params) => writeApprovalVarName(transition, principal) + params.map(p => s"[${p.name}]").mkString
  }

  private def writeApprovalVarType(transition: Transition): String =
    // Solidity doesn't allow non-elementary mapping key types
    // So we need to track combinations of parameters using a nested mapping, one layer per param
    // This is very reminiscent of Currying
    transition.parameters.getOrElse(Seq.empty[Variable]).foldRight("bool") { case (variable, current) =>
        s"mapping(${writeType(variable.ty)} => $current)"
    }

  private def writeAuthClause(transition: Transition, authDecl: AuthDecl): String =
    authDecl match {
      case AuthValue(name) => writeApprovalVarRef(transition, name)
      case AuthCombination(left, operator, right) =>
        val builder = new StringBuilder()
        left match {
          case AuthValue(name) => builder.append(writeApprovalVarRef(transition, name))
          case authCombo => builder.append(s"(${writeAuthClause(transition, authCombo)})")
        }
        operator match {
          case And => builder.append(" && ")
          case Or => builder.append(" || ")
          // This should never happen
          case _ => throw new UnsupportedOperationException(s"Operator $operator cannot be used in authorization logic")
        }
        right match {
          case AuthValue(name) => builder.append(writeApprovalVarRef(transition, name))
          case authCombo => builder.append(s"(${writeAuthClause(transition, authCombo)})")
        }

        builder.toString()
    }

  def writeSpecification(specification: Specification): String = specification match {
    case Specification(name, stateMachine, _) =>
      val builder = new StringBuilder()
      appendLine(builder, "pragma solidity >0.4.21;\n")
      appendLine(builder, s"contract $name {")

      val autoTransitions = stateMachine.transitions.filter(_.auto).foldLeft(Map.empty[String, Seq[Transition]]) { (autoTrans, transition) =>
        val originState = transition.origin.get
        autoTrans + (originState -> (autoTrans.getOrElse(originState, Seq.empty[Transition]) :+ transition))
      }

      indentationLevel += 1
      appendLine(builder, "enum State {")
      indentationLevel += 1
      stateMachine.states.zipWithIndex.foreach { case (stateName, i) =>
        appendLine(builder, if (i < stateMachine.states.size - 1) stateName + "," else stateName)
      }
      indentationLevel -= 1
      appendLine(builder, "}")

      stateMachine.fields.foreach(f => appendLine(builder, writeField(f) + ";"))
      appendLine(builder, s"State public $CURRENT_STATE_VAR;")
      builder.append(writeAuthorizationFields(stateMachine))
      builder.append("\n")

      stateMachine.transitions foreach { t => builder.append(writeTransition(t, autoTransitions)) }

      indentationLevel -= 1
      appendLine(builder, "}")
      builder.toString
  }
}
