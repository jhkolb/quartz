package edu.berkeley.cs.rise.quartz

object Solidity {
  private val SOLIDITY_VERSION: String = "0.5.7"

  private val INDENTATION_STR: String = "    "
  private val CURRENT_STATE_VAR: String = "__currentState"
  private val PARAM_HASH_PLACEHOLDER = "0"
  private val RESERVED_NAME_TRANSLATIONS: Map[String, String] = Map[String, String](
    "balance" -> "balance",
    "now" -> "now",
    "sender" -> "msg.sender",
    "tokens" -> "msg.value",
  )

  private val BUILTIN_PARAMS: Set[String] = Set("tokens")

  private var indentationLevel: Integer = 0

  private def appendLine(builder: StringBuilder, line: String): Unit =
    builder.append(s"${INDENTATION_STR * indentationLevel}$line\n")

  private def writeLine(line: String): String = s"${INDENTATION_STR * indentationLevel}$line\n"

  private def writeType(ty: DataType, payable: Boolean): String =
    ty match {
      case Identity => if (payable) "address payable" else "address"
      case IntVar => "int"
      case IntConst => throw new UnsupportedOperationException("Write type of int const")
      case UnsignedIntVar => "uint"
      case UnsignedIntConst => throw new UnsupportedOperationException("Write type of unsigned int const")
      case String => "bytes32"
      case Timestamp => "uint"
      case Bool => "bool"
      case Timespan => "uint"
      case HashValue(_) => "bytes32"
      case Mapping(keyType, valueType) => s"mapping(${writeType(keyType, payable)} => ${writeType(valueType, payable)})"
      case Sequence(elementType) => s"${writeType(elementType, payable)}[]"
      case Struct(name) => name
    }

  private def writeStructDefinition(name: String, fields: Map[String, DataType]): String = {
    val builder = new StringBuilder()
    appendLine(builder, s"struct $name {")

    indentationLevel += 1
    fields.foreach { case (fName, fTy) =>
      // TODO Determine when field must be marked payable
      appendLine(builder, s"${writeType(fTy, payable = false)} $fName;")
    }
    indentationLevel -= 1
    appendLine(builder, "}")

    builder.toString()
  }

  private def writeField(field: Variable, payable: Boolean): String =
    s"${writeType(field.ty, payable)} public ${field.name}"

  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
      case VarRef(name) => builder.append(RESERVED_NAME_TRANSLATIONS.getOrElse(name, name))
      case MappingRef(map, key) => builder.append(s"${writeExpression(map)}[${writeExpression(key)}]")
      case StructAccess(struct, fieldName) => builder.append(s"${writeExpression(struct)}.$fieldName")
      case IntConst(v) => builder.append(v)
      case UnsignedIntConst(v) => builder.append(v)
      case StringLiteral(s) => builder.append("\"" + s + "\"")
      case BoolConst(b) => builder.append(b)
      case Hash(payload) => builder.append(payload.map {
        case IntConst(v) => s"int($v)"
        case UnsignedIntConst(v) => s"uint($v)"
        case exp => writeExpression(exp)
      }.mkString("keccak256(abi.encodePacked(", ",", "))"))
      case Second => builder.append("seconds")
      case Minute => builder.append("minutes")
      case Hour => builder.append("hours")
      case Day => builder.append("days")
      case Week => builder.append("weeks")
      case LTLMax(_) | LTLMin(_) | LTLSum(_) => throw new IllegalArgumentException("LTL Expressions not used in Solidity")

      case ArithmeticOperation(left, operator, right) =>
        left match {
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case _ => builder.append(writeExpression(left))
        }

        operator match {
          case Plus => builder.append(" + ")
          case Minus => builder.append(" - ")
          case Multiply => right match {
            case Second | Minute | Hour | Day | Week => builder.append(" ")
            case _ => builder.append(" * ")
          }
          case Divide => builder.append(" / ")
          case Modulo => builder.append(" % ")
        }

        right match {
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(right))
        }

      case LogicalOperation(element, In, sequence) =>
        builder.append(s"sequenceContains(${writeExpression(sequence)}, ${writeExpression(element)})")

      case LogicalOperation(element, NotIn, sequence) =>
        builder.append(s"!(sequenceContains(${writeExpression(sequence)}, ${writeExpression(element)}))")

      case LogicalOperation(left, operator, right) =>
        left match {
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(left)})")
          case _ => builder.append(writeExpression(left))
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
          case In | NotIn | Implies => throw new IllegalArgumentException // This should never be reached
        }

        right match {
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(right))
        }

      case SequenceSize(sequence) => builder.append(s"${writeExpression(sequence)}.length")
    }

    builder.toString()
  }

  private def writeParameter(p: Variable, payable: Boolean): String = p.ty match {
    case Mapping(_, _) | Sequence(_) | Struct(_) => s"${writeType(p.ty, payable)} memory ${p.name}"
    case _ => s"${writeType(p.ty, payable)} ${p.name}"
  }

  private def writeParameters(parameters: Seq[(Variable, Boolean)]): String =
    parameters.map { case (param, payable) => writeParameter(param, payable) }.mkString(", ")

  private def writeStatement(statement: Statement, useCall: Boolean = false): String = statement match {
    case Assignment(left, right) => writeLine(s"${writeExpression(left)} = ${writeExpression(right)};")

    case Send(destination, amount, source) =>
      val destStr = destination match {
        case ArithmeticOperation(_, _, _) => s"(${writeExpression(destination)})"
        case LogicalOperation(_, _, _) => s"(${writeExpression(destination)})"
        case _ => writeExpression(destination)
      }
      source match {
        // TODO we just convert to uint as needed for now, but this assumes amount >= 0
        case None => if (useCall) {
          writeLine(s"$destStr.call.value(${writeExpression(amount)})();")
        } else {
          writeLine(s"$destStr.transfer(uint(${writeExpression(amount)}));")
        }

        case Some(s) =>
          val builder = new StringBuilder()
          appendLine(builder, s"uint __temporary = ${writeExpression(amount)};")
          appendLine(builder, s"${writeExpression(s)} = ${writeExpression(s)} - __temporary;")
          if (useCall) {
            appendLine(builder, s"$destStr.call.value(__temporary)();")
          } else {
            appendLine(builder, s"$destStr.transfer(__temporary);")
          }
          builder.toString()
      }

    case SequenceAppend(sequence, element) =>
      writeLine(s"${writeExpression(sequence)}.push(${writeExpression(element)});")

    case SequenceClear(sequence) =>
      writeLine(s"delete ${writeExpression(sequence)};")
  }

  private def writeTransition(transition: Transition, useCall: Boolean = false): String = {
    val builder = new StringBuilder()

    val paramsRepr = transition.parameters.fold("") { params =>
      // Remove parameters that are used in the original source but are built in to Solidity
      val effectiveParams = params.filter(p => !BUILTIN_PARAMS.contains(p.name))
      val payableParams = extractPayableVars(transition.body.getOrElse(Seq.empty[Statement]), effectiveParams.map(_.name).toSet)
      writeParameters(effectiveParams.zip(effectiveParams.map(p => payableParams.contains(p.name))))
    }

    val payable = if (transition.parameters.getOrElse(Seq.empty[Variable]).exists(_.name == "tokens")) {
      "payable "
    } else {
      ""
    }

    if (transition.origin.isDefined) {
      appendLine(builder, s"function ${transition.name}($paramsRepr) public $payable{")
    } else {
      appendLine(builder, s"constructor($paramsRepr) public $payable{")
    }
    indentationLevel += 1

    transition.origin.foreach { o =>
      appendLine(builder, s"require($CURRENT_STATE_VAR == State.$o);")
    }

    transition.guard.foreach { g =>
      appendLine(builder, s"require(${writeExpression(g)});")
    }

    transition.authorized.foreach { authExpr =>
      val subTerms = authExpr.basis
      if (subTerms.size == 1) {
        // Authorization clause is just a single term, which simplifies things
        // No need for persistent bookkeeping except for an "all" term
        subTerms.head match {
          case IdentityRef(identity) =>
            appendLine(builder, s"if (${RESERVED_NAME_TRANSLATIONS("sender")} != ${writeExpression(identity)}) {")
          case AuthAny(collection) =>
            appendLine(builder, s"if (!sequenceContains(${writeExpression(collection)}, ${RESERVED_NAME_TRANSLATIONS("sender")})) {")
          case AuthAll(collection) =>
            appendLine(builder, s"require(sequenceContains(${writeExpression(collection)}, ${RESERVED_NAME_TRANSLATIONS("sender")}));")
            appendLine(builder, s"${writeApprovalVarRef(transition, subTerms.head)} = true;")
            val varName = writeApprovalVarName(transition, subTerms.head)
            transition.parameters.fold {
              appendLine(builder, s"if (!allApproved(${writeExpression(collection)}, $varName, $PARAM_HASH_PLACEHOLDER)) {")
            } { params =>
              appendLine(builder, s"if (!allApproved(${writeExpression(collection)}, $varName, ${writeParamHash(params)})) {")
            }
        }
        indentationLevel += 1
        appendLine(builder, "return;")
        indentationLevel -= 1
        appendLine(builder, "}")
      } else {
        subTerms.zipWithIndex.foreach { case (subTerm, i) =>
          val conditional = if (i == 0) "if" else "else if"
          subTerm match {
            case IdentityRef(identity) =>
              appendLine(builder, s"$conditional (${RESERVED_NAME_TRANSLATIONS("sender")} == ${writeExpression(identity)}) {")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} = true;")
              indentationLevel -= 1
              appendLine(builder, "}")

            case AuthAny(collection) =>
              appendLine(builder, s"$conditional (sequenceContains(${writeExpression(collection)}, ${RESERVED_NAME_TRANSLATIONS("sender")}) {")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} = true;")
              indentationLevel -= 1
              appendLine(builder, "}")

            case AuthAll(collection) =>
              appendLine(builder, s"$conditional sequenceContains(${writeExpression(collection)}, ${RESERVED_NAME_TRANSLATIONS("sender")}) {")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} = true;")
              indentationLevel -= 1
              appendLine(builder, "}")
          }
        }
        appendLine(builder, s"if (!(${writeAuthClause(transition, authExpr)})) {")
        indentationLevel += 1
        appendLine(builder, "return;")
        indentationLevel -= 1
        appendLine(builder, "}")
      }
    }

    if (transition.origin.getOrElse("") != transition.destination) {
      // We don't need this for a self-loop
      appendLine(builder, s"$CURRENT_STATE_VAR = State.${transition.destination};")
    }

    transition.body.foreach(_.foreach(s => builder.append(writeStatement(s, useCall))))

    if (transition.origin.fold(false)(_ == transition.destination)) {
      builder.append(writeClearAuthTerms(transition))
    }
    indentationLevel -= 1
    appendLine(builder, "}")
    builder.append("\n")
    builder.toString()
  }

  private def writeAuthorizationFields(machine: StateMachine): String = {
    val builder = new StringBuilder()

    machine.transitions.foreach(trans => trans.authorized.foreach { authClause =>
      val terms = authClause.basis
      if (terms.size == 1) {
        terms.head match {
          // We don't need an explicit variable to track this
          case IdentityRef(_) | AuthAny(_) => ()
          case AuthAll(_) =>
            appendLine(builder, s"${writeApprovalVarType(trans, terms.head)} private ${writeApprovalVarName(trans, terms.head)};")
        }
      } else {
        terms.foreach { term =>
          appendLine(builder, s"${writeApprovalVarType(trans, term)} private ${writeApprovalVarName(trans, term)};")
        }
      }
    })
    builder.toString()
  }

  private def writeApprovalVarName(transition: Transition, term: AuthTerm): String =
  // Validation ensures that transition must have an origin
  // Only non-initial transitions can have authorization restrictions
    s"__${transition.name}_${transition.authTermNames(term)}Approved"

  private def writeApprovalVarType(transition: Transition, term: AuthTerm): String =
  // Use a hash of the parameters to record approvals specific to parameter combinations
    if (transition.parameters.isEmpty) {
      term match {
        case IdentityRef(_) | AuthAny(_) => "bool"
        case AuthAll(_) => "mapping(address => bool)"
      }
    } else {
      term match {
        case IdentityRef(_) | AuthAny(_) => "mapping(bytes32 => bool)"
        case AuthAll(_) => "mapping(bytes32 => mapping(address => bool))"
      }
    }

  private def writeApprovalVarRef(transition: Transition, term: AuthTerm): String =
    transition.parameters.fold {
      term match {
        case IdentityRef(_) | AuthAny(_) => writeApprovalVarName(transition, term)
        case AuthAll(_) => writeApprovalVarName(transition, term) + s"[${RESERVED_NAME_TRANSLATIONS("sender")}]"
      }
    } { params =>
      val paramHashRepr = writeParamHash(params)
      term match {
        case IdentityRef(_) | AuthAny(_) => writeApprovalVarName(transition, term) + s"[$paramHashRepr]"
        case AuthAll(_) => writeApprovalVarName(transition, term) + s"[$paramHashRepr][${RESERVED_NAME_TRANSLATIONS("sender")}]"
      }
    }

  private def writeAuthClause(transition: Transition, term: AuthExpression, depth: scala.Int = 0): String = {
    val builder = new StringBuilder()
    term match {
      case t: AuthTerm => t match {
        case IdentityRef(_) | AuthAny(_) => builder.append(writeApprovalVarRef(transition, t))

        case AuthAll(collection) =>
          // Strip out last mapping reference so we can look at all identities
          val varName = writeApprovalVarName(transition, t)
          transition.parameters.fold {
            builder.append(s"allApproved(${writeExpression(collection)}, $varName)")
          } { params =>
            builder.append(s"allApproved(${writeExpression(collection)}, $varName, ${writeParamHash(params)}")
          }
      }

      case AuthCombination(left, operator, right) =>
        if (depth > 0) {
          builder.append("(")
        }
        builder.append(writeAuthClause(transition, left, depth + 1))
        if (depth > 0) {
          builder.append(")")
        }

        operator match {
          case And => builder.append(" && ")
          case Or => builder.append(" || ")
          case Implies => throw new IllegalArgumentException
        }

        if (depth > 0) {
          builder.append("(")
        }
        builder.append(writeAuthClause(transition, right, depth + 1))
        if (depth > 0) {
          builder.append(")")
        }
    }
    builder.toString()
  }

  private def writeClearAuthTerms(transition: Transition): String = {
    val authTerms = transition.authorized.fold(Set.empty[AuthTerm])(_.basis)
    if (authTerms.size == 1) {
      authTerms.head match {
        case term@AuthAll(_) => writeClearAuthTerm(transition, term)
        case _ => ""
      }
    } else {
      val builder = new StringBuilder()
      authTerms.foreach(term => builder.append(writeClearAuthTerm(transition, term)))
      builder.toString()
    }
  }

  private def writeClearAuthTerm(transition: Transition, term: AuthTerm): String = term match {
    case IdentityRef(_) | AuthAny(_) =>
      writeLine(s"${writeApprovalVarRef(transition, term)} = false;")
    case AuthAll(collection) =>
      val varName = writeApprovalVarRef(transition, term).dropRight(s"[${RESERVED_NAME_TRANSLATIONS("sender")}]".length)
      s"""
         |for (uint i = 0; i < ${writeExpression(collection)}.length; i++) {
         |    $varName[${writeExpression(collection)}[i]] = false;
         |}
      """.trim.stripMargin.split("\n").map(INDENTATION_STR * indentationLevel + _).mkString("\n") + "\n"
  }

  private def writeSequenceContainsTest(ty: DataType): String = {
    val solidityTemplate =
      """
        |function sequenceContains({{type}}[] storage sequence, {{type}} element) private view returns (bool) {
        |    for (uint i = 0; i < sequence.length; i++) {
        |        if (sequence[i] == element) {
        |            return true;
        |        }
        |    }
        |    return false;
        |}
      """.trim.stripMargin

    val indentedTemplate = solidityTemplate.split("\n").map(INDENTATION_STR * indentationLevel + _).mkString("\n")
    indentedTemplate.replace("{{type}}", writeType(ty, payable = false))
  }

  private def writeParamHash(parameters: Seq[Variable]): String =
    s"keccak256(abi.encodePacked(${parameters.map(_.name).mkString(", ")}))"

  private def writeAllApprovesTest(withParams: Boolean): String = {
    val solidityTemplate = if (withParams) {
      """
        |function allApproved(address[] storage approvers,
        |                     mapping(bytes32 => mapping(address => bool)) storage approvals, bytes32 paramHash)
        |                     private view returns (bool) {
        |    for (uint i = 0; i < approvers.length; i++) {
        |        if (!approvals[paramHash][approvers[i]]) {
        |            return false;
        |        }
        |    }
        |    return true;
        |}
      """.trim.stripMargin
    } else {
      """
        |function allApproved(address[] storage approvers, mapping(address => bool) storage approvals))
        |         private view returns (bool) {
        |    for (uint i = 0; i < approvers.length; i++) {
        |        if (!approvals[approvers[i]]) {
        |            return false;
        |        }
        |    }
        |    return true;
        |}
      """.stripMargin
    }

    solidityTemplate.split("\n").map(INDENTATION_STR * indentationLevel + _).mkString("\n")
  }

  def writeSpecification(specification: Specification, useCall: Boolean = false): String = specification match {
    case Specification(name, stateMachine, _) =>
      val builder = new StringBuilder()
      appendLine(builder, s"pragma solidity >=$SOLIDITY_VERSION;\n")
      appendLine(builder, s"contract $name {")
      indentationLevel += 1

      val payableFields = extractPayableVars(stateMachine.flattenStatements, stateMachine.fields.map(_.name).toSet)

      stateMachine.structs.foreach { case (name, fields) => builder.append(writeStructDefinition(name, fields)) }

      appendLine(builder, "enum State {")
      indentationLevel += 1
      stateMachine.states.toSeq.zipWithIndex.foreach { case (stateName, i) =>
        appendLine(builder, if (i < stateMachine.states.size - 1) stateName + "," else stateName)
      }
      indentationLevel -= 1
      appendLine(builder, "}")

      stateMachine.fields.foreach(f => appendLine(builder, writeField(f, payableFields.contains(f.name)) + ";"))
      appendLine(builder, s"State public $CURRENT_STATE_VAR;")
      builder.append(writeAuthorizationFields(stateMachine))
      builder.append("\n")

      stateMachine.transitions foreach { t => builder.append(writeTransition(t, useCall)) }
      extractAllMembershipTypes(stateMachine).foreach(ty => builder.append(writeSequenceContainsTest(ty) + "\n"))
      builder.append("\n")

      val (transWithParams, transWithoutParams) = stateMachine.transitions.partition(_.parameters.isDefined)
      val allAuthCheck = transWithoutParams.flatMap(_.authorized).flatMap(_.basis).exists {
        case AuthAll(_) => true
        case _ => false
      }
      if (allAuthCheck) {
        builder.append(writeAllApprovesTest(false) + "\n")
      }
      val allAuthParams = transWithParams.flatMap(_.authorized).flatMap(_.basis).exists {
        case AuthAll(_) => true
        case _ => false
      }
      if (allAuthParams) {
        builder.append(writeAllApprovesTest(true) + "\n")
      }

      indentationLevel -= 1
      appendLine(builder, "}")
      builder.toString
  }

  private def extractMembershipTypes(expression: Expression): Set[DataType] = expression match {
    case LogicalOperation(left, In | NotIn, _) => Set(left.determinedType)
    case LogicalOperation(left, _, right) => extractMembershipTypes(left) ++ extractMembershipTypes(right)
    case ArithmeticOperation(left, _, right) => extractMembershipTypes(left) ++ extractMembershipTypes(right)
    case MappingRef(map, key) => extractMembershipTypes(map) ++ extractMembershipTypes(key)
    case SequenceSize(sequence) => extractMembershipTypes(sequence)
    case _ => Set.empty[DataType]
  }

  // Checks for any occurrences of the "in" or "not in" operators and the
  // element type of each sequence involved. This is used to auto-generate
  // type-specific helper functions to perform the membership check.
  private def extractAllMembershipTypes(stateMachine: StateMachine): Set[DataType] = {
    val expressionChecks = stateMachine.flattenExpressions.foldLeft(Set.empty[DataType]) { (current, exp) =>
      current.union(extractMembershipTypes(exp))
    }

    val authTerms = stateMachine.transitions.flatMap(_.authorized).flatMap(_.basis)
    val authMembershipTest = authTerms.exists {
      case AuthAll(_) | AuthAny(_) =>
        true
      case _ => false
    }

    if (authMembershipTest) {
      expressionChecks + Identity
    } else {
      expressionChecks
    }
  }

  private def extractVarNames(expression: Expression): Set[String] = expression match {
    case MappingRef(map, key) => extractVarNames(map) ++ extractVarNames(key)
    case VarRef(name) => Set(name)
    case LogicalOperation(left, _, right) => extractVarNames(left) ++ extractVarNames(right)
    case ArithmeticOperation(left, _, right) => extractVarNames(left) ++ extractVarNames(right)
    case SequenceSize(sequence) => extractVarNames(sequence)
    case _ => Set.empty[String]
  }

  private def extractPayableVars(statements: Seq[Statement], scope: Set[String] = Set.empty[String]): Set[String] = {
    val names = statements.foldLeft(Set.empty[String]) { (current, statement) =>
      statement match {
        case Send(destination, _, _) => current.union(extractVarNames(destination))
        case _ => current
      }
    }

    if (scope.nonEmpty) {
      names.intersect(scope)
    } else {
      names
    }
  }
}
