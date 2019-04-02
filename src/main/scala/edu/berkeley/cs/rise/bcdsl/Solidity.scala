package edu.berkeley.cs.rise.bcdsl

object Solidity {
  private val SOLIDITY_VERSION: String = "0.5.7"

  private val INDENTATION_STR: String = "    "
  private val CURRENT_STATE_VAR: String = "__currentState"
  private val RESERVED_NAME_TRANSLATIONS: Map[String, String] = Map[String, String](
    "balance" -> "balance",
    "now" -> "now",
    "sender" -> "msg.sender",
    "tokens" -> "int(msg.value)",
  )

  private val BUILTIN_PARAMS: Set[String] = Set("tokens")

  private var indentationLevel: Integer = 0

  private def appendLine(builder: StringBuilder, line: String): Unit =
    builder.append(s"${INDENTATION_STR * indentationLevel}$line\n")

  private def writeLine(line: String): String = s"${INDENTATION_STR * indentationLevel}$line\n"

  private def writeType(ty: DataType, payable: Boolean): String =
    ty match {
      case Identity => if (payable) "address payable" else "address"
      case Int => "int"
      case String => "bytes32"
      case Timestamp => "uint"
      case Bool => "bool"
      case Timespan => "uint"
      case Mapping(keyType, valueType) => s"mapping(${writeType(keyType, payable)} => ${writeType(valueType, payable)})"
      case Sequence(elementType) => s"${writeType(elementType, payable)}[]"
    }

  private def writeField(field: Variable, payable: Boolean): String =
    s"${writeType(field.ty, payable)} public ${field.name}"

  private def writeExpression(expression: Expression): String = {
    val builder = new StringBuilder()
    expression match {
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
          case In | NotIn => throw new IllegalArgumentException // This should never be reached
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

  private def writeParameter(p: Variable, payable: Boolean): String = s"${writeType(p.ty, payable)} ${p.name}"

  private def writeParameters(parameters: Seq[(Variable, Boolean)]): String =
    parameters.map{ case (param, payable) => writeParameter(param, payable) }.mkString(", ")

  private def writeAssignable(assignable: Assignable): String = assignable match {
    case VarRef(name) => name
    case MappingRef(mapName, key) => s"$mapName[${writeExpression(key)}]"
  }

  private def writeStatement(statement: Statement): String = statement match {
    case Assignment(left, right) => writeLine(s"${writeAssignable(left)} = ${writeExpression(right)};")

    case Send(destination, amount, source) =>
      val destStr = destination match {
        case ArithmeticOperation(_, _, _) => s"(${writeExpression(destination)})"
        case LogicalOperation(_, _, _) => s"(${writeExpression(destination)})"
        case _ => writeExpression(destination)
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

    case SequenceAppend(sequence, element) =>
      writeLine(s"${writeExpression(sequence)}.push(${writeExpression(element)});")
  }

  private def writeTransition(transition: Transition, autoTransitions: Map[String, Seq[Transition]]): String = {
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

    transition.authorized.foreach { authTerm =>
      val subTerms = authTerm.flatten
      if (subTerms.size == 1) {
        subTerms.head match {
          case IdentityLiteral(identity) =>
            appendLine(builder, s"require(${RESERVED_NAME_TRANSLATIONS("sender")} == $identity);")
          case AuthAny(collectionName) =>
            appendLine(builder, s"require(sequenceContains($collectionName, ${RESERVED_NAME_TRANSLATIONS("sender")}));")
          case AuthAll(collectionName) =>
            appendLine(builder, s"${writeApprovalVarRef(transition, subTerms.head)} = true;")
            val varName = writeApprovalVarName(transition, subTerms.head).dropRight(s"[${RESERVED_NAME_TRANSLATIONS("sender")}]".length())
            appendLine(builder, s"require(allApproved($varName, $collectionName);")
        }
      } else {
        subTerms.zipWithIndex.foreach { case (subTerm, i) =>
          val conditional = if (i == 0) "if" else "else if"
          subTerm match {
            case IdentityLiteral(identity) =>
              appendLine(builder, s"$conditional (${RESERVED_NAME_TRANSLATIONS("sender")} == $identity) {")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} = true;")
              indentationLevel -= 1
              appendLine(builder, "}")

            case AuthAny(collectionName) =>
              appendLine(builder, s"$conditional (sequenceContains($collectionName, ${RESERVED_NAME_TRANSLATIONS("sender")}) {")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} = true;")
              indentationLevel -= 1
              appendLine(builder, "}")

            case AuthAll(collectionName) =>
              appendLine(builder, s"$conditional sequenceContains($collectionName, ${RESERVED_NAME_TRANSLATIONS("sender")}) {")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} = true;")
              indentationLevel -= 1
              appendLine(builder, "}")
          }
        }
        appendLine(builder, s"require(${writeAuthClause(transition, authTerm)});")
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

    machine.transitions.foreach(trans => trans.authorized.foreach { authClause =>
      val terms = authClause.flatten
      if (terms.size == 1) {
        terms.head match {
          // We don't need an explicit variable to track this
          case IdentityLiteral(_) | AuthAny(_) => ()
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
    s"__${transition.name}_${term.getReferencedName}Approved"

  private def writeApprovalVarType(transition: Transition, term: AuthTerm): String = {
    // Solidity doesn't allow non-elementary mapping key types
    // So we need to track combinations of parameters using a nested mapping, one layer per param
    val startingPoint = term match {
      case IdentityLiteral(_) | AuthAny(_) => "bool"
      case AuthAll(_) => "mapping(address => bool)"
    }
    transition.parameters.getOrElse(Seq.empty[Variable]).foldRight(startingPoint) { case (param, current) =>
      s"mapping(${writeType(param.ty, payable = false)} => $current)"
    }
  }

  private def writeApprovalVarRef(transition: Transition, term: AuthTerm): String = term match {
    case IdentityLiteral(_) | AuthAny(_) => transition.parameters match {
      case None => writeApprovalVarName(transition, term)
      case Some(params) => writeApprovalVarName(transition, term) + params.map(p => s"[${p.name}]").mkString
    }

    case AuthAll(_) =>
      val senderName = RESERVED_NAME_TRANSLATIONS("sender")
      val effectiveParams = transition.parameters.getOrElse(Seq.empty[Variable]) :+ Variable(senderName, Identity)
      writeApprovalVarName(transition, term) + effectiveParams.map(p => s"[${p.name}]").mkString
  }

  private def writeAuthClause(transition: Transition, term: AuthExpression, depth: Int = 0): String = {
    val builder = new StringBuilder()
    term match {
      case t: AuthTerm => t match {
        case IdentityLiteral(_) | AuthAny(_) => builder.append(writeApprovalVarRef(transition, t))

        case AuthAll(collectionName) =>
          // Strip out last mapping reference so we can look at all identities
          val senderName = RESERVED_NAME_TRANSLATIONS("sender")
          val varName = writeApprovalVarRef(transition, t).dropRight(s"[$senderName]".length())
          builder.append(s"allApproved($varName, $collectionName)")
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

  def writeSpecification(specification: Specification): String = specification match {
    case Specification(name, stateMachine, _) =>
      val builder = new StringBuilder()
      appendLine(builder, s"pragma solidity >=$SOLIDITY_VERSION;\n")
      appendLine(builder, s"contract $name {")

      val autoTransitions = stateMachine.transitions.filter(_.auto).foldLeft(Map.empty[String, Seq[Transition]]) { (autoTrans, transition) =>
        val originState = transition.origin.get
        autoTrans + (originState -> (autoTrans.getOrElse(originState, Seq.empty[Transition]) :+ transition))
      }

      val payableFields = extractPayableVars(stateMachine.flattenStatements, stateMachine.fields.map(_.name).toSet)

      indentationLevel += 1
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

      stateMachine.transitions foreach { t => builder.append(writeTransition(t, autoTransitions)) }
      extractAllMembershipTypes(stateMachine).foreach(ty => builder.append(writeSequenceContainsTest(ty) + "\n"))

      indentationLevel -= 1
      appendLine(builder, "}")
      builder.toString
  }

  private def extractMembershipTypes(expression: Expression): Set[DataType] = expression match {
    case LogicalOperation(left, In | NotIn, _) => Set(left.determinedType)
    case LogicalOperation(left, _, right) => extractMembershipTypes(left) ++ extractMembershipTypes(right)
    case ArithmeticOperation(left, _, right) => extractMembershipTypes(left) ++ extractMembershipTypes(right)
    case MappingRef(_, key) => extractMembershipTypes(key)
    case SequenceSize(sequence) => extractMembershipTypes(sequence)
    case _ => Set.empty[DataType]
  }

  private def extractAllMembershipTypes(stateMachine: StateMachine): Set[DataType] = {
    val expressionChecks = stateMachine.flattenExpressions.foldLeft(Set.empty[DataType]) { (current, exp) =>
      current.union(extractMembershipTypes(exp))
    }

    val authTerms = stateMachine.transitions.flatMap(_.guard).flatMap(extractMembershipTypes)
    val authMembershipTest = authTerms.contains{ authTerm : AuthTerm =>
      authTerm match {
        case AuthAll(_) | AuthAny(_) => true
        case _ => false
      }
    }

    if (authMembershipTest) {
      expressionChecks + Identity
    } else {
      expressionChecks
    }
  }

  private def extractVarNames(expression: Expression): Set[String] = expression match {
    case MappingRef(mapName, key) => extractVarNames(key) + mapName
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
