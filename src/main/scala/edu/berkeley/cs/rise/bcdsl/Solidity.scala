package edu.berkeley.cs.rise.bcdsl

object Solidity {
  def writeExpression(expression: Expression): String = {
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
      }

      case ArithmeticExpression(left, operator, right) =>
        builder.append(s"(${writeExpression(left)})")
        operator match {
          case Plus => builder.append (" + ")
          case Minus => builder.append (" - ")
          case Multiply => builder.append (" * ")
          case Divide => builder.append (" / ")
        }
        builder.append(s"(${writeExpression(right)})")

      case LogicalExpression(left, operator, right) =>
        builder.append(s"(${writeExpression(left)}")
        operator match {
          case LessThan => builder.append(" < ")
          case LessThanOrEqual => builder.append(" <= ")
          case Equal => builder.append(" == ")
          case NotEqual => builder.append(" != ")
          case GreaterThanOrEqual => builder.append(" >= ")
          case GreaterThan => builder.append(" > ")
        }
        builder.append(s"(${writeExpression(right)})")
    }

    builder.toString()
  }
}
