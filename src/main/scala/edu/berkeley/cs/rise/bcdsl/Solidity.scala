package edu.berkeley.cs.rise.bcdsl

object Solidity {
  private val INDENTATION_STR: String = "    "

  private def writeField(field: Field): String = {
    val builder = new StringBuilder()

    builder.append(INDENTATION_STR)
    field.ty match {
      case Identity => builder.append("address")
      case Int => builder.append("int")
      case String => builder.append("bytes32")
      case Timestamp => builder.append("uint")
      case Bool => builder.append("bool")
      case Timespan => builder.append("uint")
    }
    builder.append(s" public ${field.name};\n")

    builder.toString()
  }

  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
      case ValueExpression(value) => value match {
        case FieldRef(name) => builder.append(name)
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

  private def writeTransition(transition: Transition): String = {
    val builder = new StringBuilder()

    builder.append(INDENTATION_STR)
    transition.origin match {
      case None => builder.append("constructor() public {\n")
      case Some(o) => builder.append(s"function ${o}_to_${transition.destination}() public {\n")
    }

    transition.guard match {
      case None => ()
      case Some(g) =>
        builder.append(INDENTATION_STR * 2)
        builder.append(s"require(${writeExpression(g)});\n")
    }
    builder.append(INDENTATION_STR * 2)
    builder.append(s"currentState = State.${transition.destination};\n")

    transition.body match {
      case None => ()
      case Some(assignments) => assignments foreach { assignment =>
        builder.append(INDENTATION_STR * 2)
        builder.append(s"${assignment.name} = ${writeExpression(assignment.value)};\n")
      }
    }
    builder.append(INDENTATION_STR)
    builder.append("}\n\n")

    builder.toString()
  }

  def writeStateMachine(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    builder.append("pragma solidity ^0.4.21;\n\n")
    builder.append("contract AutoGen {\n")

    val states: Set[String] = stateMachine.transitions.foldLeft(Set.empty[String]) { (states, transition) =>
      transition.origin match {
        case None => states + transition.destination
        case Some(o) => states + (o, transition.destination)
      }
    }
    builder.append(INDENTATION_STR)
    builder.append("enum State {\n")
    builder.append(INDENTATION_STR * 2)
    builder.append(states.mkString(",\n" + (INDENTATION_STR * 2)))
    builder.append("\n")
    builder.append(INDENTATION_STR + "}\n")

    stateMachine.fields foreach { f => builder.append(writeField(f)) }
    builder.append(INDENTATION_STR)
    builder.append("State public currentState;\n\n")

    stateMachine.transitions foreach { t => builder.append(writeTransition(t)) }

    builder.append("}")
    builder.toString
  }
}
