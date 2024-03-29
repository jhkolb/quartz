package edu.berkeley.cs.rise.quartz

import java.io.File
import java.nio.file.Files

import scala.io.Source
import scala.sys.process._

object TLA {
  private val MODEL_CONSTANTS: Map[String, String] = Map(elems =
    "MAX_TIMESTEP" -> "3",
    "MAX_ELAPSED_TIME" -> "11",
    "MIN_INT" -> "-5",
    "MAX_INT" -> "5",
    "MAX_CALL_DEPTH" -> "3",
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

  // TODO make these user-visible configuration parameters
  private[quartz] val NUM_IDENTITIES = 3
  private[quartz] val NUM_STRINGS = 1

  private def mangleName(name: String): String = "__" + name.toLowerCase

  def translatePlusCal(moduleName: String, plusCal: String): String = {
    val tempDir = Files.createTempDirectory("tlaGen")
    val tempTlaFile = new File(tempDir.toString, s"$moduleName.tla")
    Utils.writeStringToFile(tempTlaFile, plusCal)

    // '!!' Runs the specified command and retrieves string output
    val translationOutput = Process(s"pcal -nocfg ${tempTlaFile.getAbsolutePath}").lineStream_!.mkString("\n")
    if (!translationOutput.endsWith(s"New file ${tempTlaFile.getAbsolutePath} written.")) {
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
        builder.append(s"__property_$i == ${writeLTLProperty(prop, specification.stateMachine.states)}\n")
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

  private def writeLTLProperty(property: LTLProperty, stateNames: Set[String]): String = property match {
    case LTLProperty(op, Left(prop)) => s"${writeLTLOperator(op)}(${writeLTLProperty(prop, stateNames)})"
    case LTLProperty(op, Right(expr)) => s"${writeLTLOperator(op)}(${writeExpression(expr, stateNames)})"
  }

  private def writeLTLOperator(operator: LTLOperator): String = operator match {
    case Always => "[]"
    case Eventually => "<>"
  }

  private[quartz] def flattenName(assignable: Assignable): String = assignable match {
    case VarRef(name) => name
    case MappingRef(map, key) => throw new NotImplementedError("flattenName(MappingRef")
    case StructAccess(struct, field) => s"${flattenName(struct)}_$field"
  }

  // TODO deal with code duplication between this and PlusCal
  private def writeExpression(expression: Expression, stateNames: Set[String]): String =
    expression match {
      case VarRef(name) => if (stateNames.contains(name)) {
        s"(__currentState = ${name.toUpperCase})"
      } else {
        RESERVED_NAME_TRANSLATIONS.getOrElse(name, name)
      }
      case MappingRef(map, key) => s"${writeExpression(map, stateNames)}[${writeExpression(key, stateNames)}]"
      case StructAccess(struct, field) => s"${writeExpression(struct, stateNames)}.$field"
      case IntConst(v) => v.toString
      case UnsignedIntConst(v) => v.toString
      case StringLiteral(s) => "\"" + s + "\""
      case BoolConst(b) => b.toString.toUpperCase
      case Hash(payload) => payload.map(writeExpression(_, stateNames)).mkString("<<", ", ", ">>")
      case SequenceSize(sequence) => s"Len(${writeExpression(sequence, stateNames)})"
      case Second => "1"
      case Minute => "60"
      case Hour => "3600"
      case Day => "86400"
      case Week => "604800"
      case LTLMax(body) => s"__max_${flattenName(body)}"
      case LTLMin(body) => s"__min_${flattenName(body)}"
      case LTLSum(body) => body match {
        case v @ VarRef(name) => v.determinedType match {
          case Sequence(_) => s"SeqSum($name)"
          case Mapping(_, _) => s"MapSum($name)"
          case _ => throw new IllegalArgumentException
        }
        case _ => throw new IllegalArgumentException
      }

      case ArithmeticOperation(left, op, right) =>
        val builder = new StringBuilder()
        left match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(left, stateNames)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(left, stateNames)})")
          case _ => builder.append(writeExpression(left, stateNames))
        }

        op match {
          case Plus => builder.append(" + ")
          case Minus => builder.append(" - ")
          case Multiply => builder.append(" * ")
          case Divide => builder.append(" / ")
          case Modulo => builder.append(" % ")
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right, stateNames)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right, stateNames)})")
          case _ => builder.append(writeExpression(left, stateNames))
        }

        builder.toString()

      case LogicalOperation(element, op@(In | NotIn), sequence) =>
        val builder = new StringBuilder()
        if (op == NotIn) {
          builder.append("~(")
        }
        builder.append(s"\\E x \\in DOMAIN ${writeExpression(sequence, stateNames)}: ")
        builder.append(s"${writeExpression(sequence, stateNames)}[x] = ${writeExpression(element, stateNames)}")
        if (op == NotIn) {
          builder.append(")")
        }
        builder.toString()

      case LogicalOperation(left, op, right) =>
        val builder = new StringBuilder()
        left match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(left, stateNames)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(left, stateNames)})")
          case _ => builder.append(writeExpression(left, stateNames))
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
          case Implies => builder.append(" => ")
          case In | NotIn => throw new IllegalArgumentException // This should never be reached
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right, stateNames)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right, stateNames)})")
          case _ => builder.append(writeExpression(right, stateNames))
        }

        builder.toString()
    }
}
