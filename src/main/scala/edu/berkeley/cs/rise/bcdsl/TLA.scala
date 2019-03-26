package edu.berkeley.cs.rise.bcdsl

object TLA {
  private val MODEL_CONSTANTS: Map[String, String] = Map(elems =
    "MAX_TIMESTEP" -> "5",
    "MAX_ELAPSED_TIME" -> "11",
    "MIN_INT" -> "-1000",
    "MAX_INT" -> "1000",
    "MAX_CALL_DEPTH" -> "5",
  )

  private val MODEL_CONSTRAINTS: Seq[String] = Seq(
    s"${PlusCal.CALL_DEPTH_VAR} <= __max_call_depth",
    s"${PlusCal.CURRENT_TIME_VAR} <= __max_elapsed_time",
  )

  private val ZERO_IDENTITY_NAME = "ZERO_IDENT"

  private val RESERVED_NAME_TRANSLATIONS = Map[String, String](
    "balance" -> "balance",
    "sender" -> "sender",
    "now" -> "__currentTime",
  )

  private def mangleName(name: String): String = "__" + name.toLowerCase

  def writeSpecificationToAux(specification: Specification): String = specification match {
    case Specification(name, _, invariants) =>
      val builder = new StringBuilder()
      builder.append("---- MODULE MC ----\n")
      builder.append(s"EXTENDS $name, TLC\n")

      MODEL_CONSTANTS.foreach { case (constName, constValue) =>
        builder.append(s"${mangleName(constName)} == $constValue\n")
      }

      invariants.foreach(_.zipWithIndex.foreach { case (prop, i) =>
        builder.append(s"__property_$i == ${writeLTLProperty(prop)}\n")
      })

      // Write constraints on model checking domain
      MODEL_CONSTRAINTS.zipWithIndex.foreach { case (constraint, i) =>
        builder.append(s"__constraint_$i == $constraint\n")
      }

      builder.append("=" * 10)
      builder.toString()
  }

  def writeSpecificationToConfig(specification: Specification): String = specification match {
    case Specification(_, stateMachine, invariants) =>
      val builder = new StringBuilder()

      builder.append("CONSTANT defaultInitValue = defaultInitValue\n")
      // Pull in Spec definition from PlusCal file
      builder.append("SPECIFICATION Spec\n")
      // Treat states as symbolic constants
      stateMachine.states.map(_.toUpperCase).foreach(name => builder.append(s"CONSTANT $name = $name\n"))

      // Treat identities as symbolic constants, add in zero identity
      stateMachine.fields.filter(_.ty == Identity).map(_.name.toUpperCase()).foreach(id => builder.append(s"CONSTANT $id = $id\n"))
      builder.append(s"CONSTANT $ZERO_IDENTITY_NAME = $ZERO_IDENTITY_NAME\n")

      // Load in constants from auxiliary file
      MODEL_CONSTANTS.keys.foreach(constName => builder.append(s"CONSTANT $constName <- ${mangleName(constName)}\n"))

      invariants.foreach(_.indices.foreach(i => builder.append(s"PROPERTY __property_$i\n")))
      MODEL_CONSTRAINTS.indices.foreach(i => builder.append(s"CONSTRAINT __constraint_$i\n"))

      builder.toString()
  }

  private def writeLTLProperty(property: LTLProperty): String = property match {
    case LTLProperty(op, Left(prop)) => s"${writeLTLOperator(op)}(${writeLTLProperty(prop)})\n"
    case LTLProperty(op, Right(expr)) => expr match {
      case VarRef(name) => s"${writeLTLOperator(op)}(__currentState = ${name.toUpperCase()})"
      case _ => s"${writeLTLOperator(op)}(${writeExpression(expr)})"
    }
  }

  private def writeLTLOperator(operator: LTLOperator): String = operator match {
    case Always => "[]"
    case Eventually => "<>"
  }

  // TODO deal with code duplication between this and PlusCal
  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
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

      case ArithmeticOperation(left, op, right) =>
        left match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case _ => builder.append(writeExpression(left))
        }

        op match {
          case Plus => builder.append(" + ")
          case Minus => builder.append(" - ")
          case Multiply => builder.append(" * ")
          case Divide => builder.append(" / ")
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(left))
        }

      case LogicalOperation(element, op @ (In | NotIn), sequence) =>
        if (op == NotIn) {
          builder.append("~(")
        }
        builder.append(s"\\E x \\in DOMAIN ${writeExpression(sequence)}: ")
        builder.append(s"${writeExpression(sequence)}[x] = ${writeExpression(element)}")
        if (op == NotIn) {
          builder.append(")")
        }

      case LogicalOperation(left, op, right) =>
        left match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case _ => builder.append(writeExpression(left))
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
          case In | NotIn => throw new IllegalArgumentException // This should never be reached
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(left))
        }

      case SequenceSize(sequence) => builder.append(s"Len(${writeExpression(sequence)})")
    }

    builder.toString()
  }
}


