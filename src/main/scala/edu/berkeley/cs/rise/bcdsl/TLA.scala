package edu.berkeley.cs.rise.bcdsl

import java.io.File
import java.nio.file.Files

import scala.io.Source
import scala.sys.process._

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

  private[bcdsl] val NUM_IDENTITIES = 3

  private def mangleName(name: String): String = "__" + name.toLowerCase

  def translatePlusCal(moduleName: String, plusCal: String): String = {
    val tempDir = Files.createTempDirectory("tlaGen")
    val tempTlaFile = new File(tempDir.toString, s"$moduleName.tla")
    Utils.writeStringToFile(tempTlaFile, plusCal)

    // '!!' Runs the specified command and retrieves string output
    val translationOutput = s"pcal -nocfg ${tempTlaFile.getAbsolutePath}".!!
    if (!translationOutput.endsWith(s"New file ${tempTlaFile.getAbsolutePath} written.\n")) {
      throw new RuntimeException(s"PlusCal translator failed: $translationOutput")
    }

    val tlaSource = Source.fromFile(tempTlaFile)
    val translation = tlaSource.mkString
    tlaSource.close()
    translation
  }

  def writeSpecificationToAux(specification: Specification): String = specification match {
    case Specification(name, _, invariants) =>
      val builder = new StringBuilder()
      builder.append(s"---- MODULE ${specification.name}MC ----\n")
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
      1.to(NUM_IDENTITIES).foreach { i =>
        builder.append(s"CONSTANT __ident$i = __ident$i\n")
      }
      builder.append(s"CONSTANT $ZERO_IDENTITY_NAME = $ZERO_IDENTITY_NAME\n")

      // Load in constants from auxiliary file
      MODEL_CONSTANTS.keys.foreach(constName => builder.append(s"CONSTANT $constName <- ${mangleName(constName)}\n"))

      invariants.foreach(_.indices.foreach(i => builder.append(s"PROPERTY __property_$i\n")))
      MODEL_CONSTRAINTS.indices.foreach(i => builder.append(s"CONSTRAINT __constraint_$i\n"))

      builder.toString()
  }

  // Use some ugly regex replacing for this
  // Scala's regex engine requires some additional options to make this work
  // (?m) enables multi-line mode (so ^ and $ match on lines, not whole string)
  // (?s) enables the dot character to match on newlines as well
  def modifyGeneratedTLA(original: String): String = {
    original.
      // Inside "Throw" procedure, don't return normally. Jump back to main loop
      replaceFirst("(?ms)(^Throw == /\\\\ pc = \"Throw\".*?)(pc' = Head\\(stack\\)\\.pc)",
        "$1pc' = \"Loop\"").
      // Also, discard all of the call sequence so far
      replaceFirst("(?ms)(^Throw ==.*?/\\\\ pc' = \"Loop\"\\s+/\\\\ )(stack' = Tail\\(stack\\))",
        "$1stack' = <<>>")
  }

  private def writeLTLProperty(property: LTLProperty): String = property match {
    case LTLProperty(op, Left(prop)) => s"${writeLTLOperator(op)}(${writeLTLProperty(prop)})"
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
      case MappingRef(map, key) => builder.append(s"${writeExpression(map)}[${writeExpression(key)}]")
      case ScopedParamRef(transition, parameter) => builder.append(transition + "_" + parameter)
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
          case _ => builder.append(writeExpression(right))
        }

      case SequenceSize(sequence) => builder.append(s"Len(${writeExpression(sequence)})")
    }

    builder.toString()
  }
}
