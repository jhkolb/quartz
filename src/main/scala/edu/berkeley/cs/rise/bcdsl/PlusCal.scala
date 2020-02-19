package edu.berkeley.cs.rise.bcdsl

object PlusCal {

  private val INDENTATION_STR = "    "
  private val BUILT_IN_CONSTANTS = Seq("MAX_INT, MIN_INT, MAX_TIMESTEP", "MAX_ELAPSED_TIME", "MAX_CALL_DEPTH")
  private val CONTRACT_BALANCE_VAR = "balance"
  private[bcdsl] val CALL_DEPTH_VAR = "__contractCallDepth"
  private[bcdsl] val CURRENT_TIME_VAR = "__currentTime"
  private val CURRENT_STATE_VAR = "__currentState"
  private val STATE_STASH_VAR = "__stateStash"

  private var labelCounter = 0
  private var indentationLevel = 0

  private val RESERVED_NAME_TRANSLATIONS: Map[String, String] = Map[String, String](
    "balance" -> "balance",
    "sender" -> "sender",
    "now" -> "__currentTime",
  )

  private def appendLine(builder: StringBuilder, line: String): Unit =
    builder.append(s"${INDENTATION_STR * indentationLevel}$line\n")

  private def writeLine(line: String): String = s"${INDENTATION_STR * indentationLevel}$line\n"

  private def writeDomain(ty: DataType): String = ty match {
    case Identity => "IDENTITIES \\ {ZERO_IDENT}"
    case Int => "MIN_INT..MAX_INT"
    case Bool => "{ TRUE, FALSE }"
    case Timestamp => "0..MAX_INT"
    case Timespan => "0..MAX_INT"
    case String => throw new NotImplementedError("Strings have infinite domain") // TODO
    case Mapping(keyType, valueType) => s"[ x \\in ${writeDomain(keyType)} -> ${writeDomain(valueType)} ]"
    case Sequence(elementType) => s"[ x \\in 1..MAX_INT -> ${writeDomain(elementType)} ]"
  }

  private def writeZeroElement(ty: DataType): String = ty match {
    case Identity => "ZERO_IDENT"
    case Int => "0"
    case String => "\"\""
    case Timestamp => "0"
    case Bool => "FALSE"
    case Timespan => "0"
    case Mapping(keyType, valueType) => s"[ x \\in ${writeDomain(keyType)} |-> ${writeZeroElement(valueType)} ]"
    case Sequence(_) => "<<>>"
  }

  private def writeField(field: Variable): String = s"${field.name} = ${writeZeroElement(field.ty)}"

  private def writeApprovalVarName(transition: Transition, term: AuthTerm): String =
  // Only non-initial transitions can have authorization clause
    s"${transition.name}_${term.getReferencedName}_approved"

  private def writeApprovalVarRef(transition: Transition, term: AuthTerm): String = term match {
    case IdentityLiteral(_) | AuthAny(_) => transition.parameters match {
      case None => writeApprovalVarName(transition, term)
      case Some(params) =>
        val paramsStructRepr = "[" + params.map(p => s"${p.name} |-> ${p.name}").mkString(", ") + "]"
        s"${writeApprovalVarName(transition, term)}[ $paramsStructRepr ]"
    }
    case AuthAll(_) =>
      val effectiveParams = Variable(RESERVED_NAME_TRANSLATIONS("sender"), Identity) +: transition.parameters.getOrElse(Seq.empty[Variable])
      val paramsStructRepr = "[" + effectiveParams.map(p => s"${p.name} |-> ${p.name}").mkString(", ") + "]"
      s"${writeApprovalVarName(transition, term)}[ $paramsStructRepr ]"
  }

  private def writeApprovalVarInit(transition: Transition, term: AuthTerm): String = transition.parameters match {
    case None => "FALSE"
    case Some(params) =>
      val effectiveParams = term match {
        case IdentityLiteral(_) | AuthAny(_) => params
        case AuthAll(_) => Variable(RESERVED_NAME_TRANSLATIONS("sender"), Identity) +: params
      }
      val paramsStructRepr = "[" + effectiveParams.map(p => s"${p.name}: ${writeDomain(p.ty)}").mkString(", ") + "]"
      s"[ x \\in $paramsStructRepr |-> FALSE]"
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
            appendLine(builder, s"${writeApprovalVarName(trans, terms.head)} = ${writeApprovalVarInit(trans, terms.head)};")
        }
      } else {
        terms.foreach { term =>
          appendLine(builder, s"${writeApprovalVarName(trans, term)} = ${writeApprovalVarInit(trans, term)};")
        }
      }
    })

    builder.toString()
  }

  private def getAuthorizationFieldNames(machine: StateMachine): Seq[String] = machine.transitions.flatMap { transition =>
    transition.authorized.fold(Seq.empty[String]) { authClause =>
      val terms = authClause.flatten
      if (terms.size == 1) {
        terms.head match {
          case IdentityLiteral(_) | AuthAny(_) => Seq.empty[String]
          case term@AuthAll(_) => Seq(writeApprovalVarName(transition, term))
        }
      } else {
        terms.toSeq.map(writeApprovalVarName(transition, _))
      }
    }
  }

  private def getStashedVariables(stateMachine: StateMachine): Seq[String] =
    stateMachine.fields.map(_.name) ++ getAuthorizationFieldNames(stateMachine) ++
      Seq(CONTRACT_BALANCE_VAR, CURRENT_STATE_VAR)

  private def writeStateStash(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    appendLine(builder, s"$STATE_STASH_VAR = [")
    indentationLevel += 1
    val allVarNames = getStashedVariables(stateMachine)
    allVarNames.dropRight(1).foreach { name =>
      appendLine(builder, s"$name |-> $name,")
    }
    appendLine(builder, s"${allVarNames.last} |-> ${allVarNames.last}")
    indentationLevel -= 1
    appendLine(builder, "];")
    builder.toString()
  }

  private def writeAuthClause(transition: Transition, term: AuthExpression, depth: Int = 0): String = {
    val builder = new StringBuilder()
    term match {
      case t: AuthTerm => t match {
        case IdentityLiteral(_) | AuthAny(_) => builder.append(writeApprovalVarRef(transition, t))

        case AuthAll(collectionName) =>
          val paramReprs = "sender |-> s" +: transition.parameters.getOrElse(Seq.empty[Variable]).map(p => s"${p.name} |-> ${p.name}")
          builder.append(s"\\A s \\in $collectionName: ${writeApprovalVarName(transition, t)}[${paramReprs.mkString(", ")}]")
      }

      case AuthCombination(left, op, right) =>
        if (depth > 0) {
          builder.append("(")
        }
        builder.append(writeAuthClause(transition, left, depth + 1))
        if (depth > 0) {
          builder.append(")")
        }

        op match {
          case And => builder.append(" /\\ ")
          case Or => builder.append(" \\/ ")
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
    val authTerms = transition.authorized.fold(Set.empty[AuthTerm])(_.flatten)
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
    case IdentityLiteral(_) | AuthAny(_) =>
      writeLine(s"${writeApprovalVarRef(transition, term)} := FALSE;")
    case AuthAll(collectionName) =>
      val varName = writeApprovalVarName(transition, term)
      val paramReprs = transition.parameters.fold(Seq.empty[String])(_.map(p => s"${p.name} |-> ${p.name}"))
      val effectiveParamReprs = s"sender \\in $collectionName" +: paramReprs
      writeLine(s"$varName[${effectiveParamReprs.mkString(", ")}] := FALSE;")
  }

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
          case Divide => builder.append(" \\div ")
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(right))
        }

      case LogicalOperation(element, op@(In | NotIn), sequence) =>
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

  private def writeAssignable(assignable: Assignable): String = assignable match {
    case VarRef(name) => name
    case MappingRef(map, key) => s"${writeExpression(map)}[${writeExpression(key)}]"
    case ScopedParamRef(transition, parameter) => transition + "_" + parameter
  }

  private def writeStatement(statement: Statement): String = statement match {
    case Assignment(left, right) => writeLine(s"${writeAssignable(left)} := ${writeExpression(right)};")

    case Send(destination, amount, source) => source match {
      case None => writeLine(s"call sendTokens(${writeExpression(destination)}, ${writeExpression(amount)});") + nextLabel() + "\n"
      case Some(s) =>
        val builder = new StringBuilder()
        appendLine(builder, s"__temporary := ${writeExpression(amount)};")
        appendLine(builder, s"${writeAssignable(s)} := ${writeAssignable(s)} - __temporary;")
        appendLine(builder, s"call sendTokens(${writeExpression(destination)}, __temporary);")
        builder.append(nextLabel() + "\n")
        builder.toString()
    }

    case SequenceAppend(sequence, element) =>
      writeLine(s"${writeExpression(sequence)} := Append(${writeExpression(sequence)}, ${writeExpression(element)});")

    case SequenceClear(sequence) =>
      writeLine(s"${writeExpression(sequence)} := <<>>;")
  }

  private def nextLabel(): String = {
    val currentLabelIndex = labelCounter
    labelCounter += 1
    s"L$currentLabelIndex:"
  }

  private def writeTransition(transition: Transition, autoTransitions: Map[String, Seq[Transition]]): String = {
    val builder = new StringBuilder()
    val effectiveParameters = Variable("sender", Identity) +: transition.parameters.getOrElse(Seq.empty[Variable])
    val paramsRepr = effectiveParameters.map(_.name).mkString(", ")
    appendLine(builder, s"procedure ${transition.name}($paramsRepr) begin ${transition.name}:")
    indentationLevel += 1

    transition.origin.foreach { o =>
      appendLine(builder, s"if $CURRENT_STATE_VAR /= ${o.toUpperCase} then")
      indentationLevel += 1
      appendLine(builder, "return;")
      indentationLevel -= 1
      appendLine(builder, "end if;")
      builder.append(nextLabel() + "\n")
    }

    // Interpose automatic transitions, if any
    val outgoingAutoTransitions = transition.origin.flatMap(autoTransitions.get)
    outgoingAutoTransitions.foreach(_.filter(_ != transition).zipWithIndex.foreach { case (t, idx) =>
      val g = t.guard.get // Auto transitions must have a guard
      if (idx == 0) {
        appendLine(builder, s"if ${writeExpression(g)} then")
      } else {
        appendLine(builder, s"else if ${writeExpression(g)} then")
      }
      indentationLevel += 1

      if (t.destination != t.origin.get) {
        appendLine(builder, s"$CURRENT_STATE_VAR := ${t.destination.toUpperCase};")
      }
      t.body.foreach(_.foreach(s => builder.append(writeStatement(s))))
      appendLine(builder, "return;")
      indentationLevel -= 1
      appendLine(builder, "end if;")
      builder.append(nextLabel() + "\n")
    })

    transition.guard.foreach { g =>
      appendLine(builder, s"if ~(${writeExpression(g)}) then")
      indentationLevel += 1
      if (transition.origin.isEmpty) {
        // Contract would throw exception if initial transition guard not satisfied
        appendLine(builder, "goto Done;")
      } else {
        appendLine(builder, "return;")
      }
      indentationLevel -= 1
      appendLine(builder, "end if;")
      builder.append(nextLabel() + "\n")
    }

    transition.authorized.foreach { authTerm =>
      val subTerms = authTerm.flatten
      if (subTerms.size == 1) {
        subTerms.head match {
          case IdentityLiteral(identity) =>
            appendLine(builder, s"if sender /= $identity then")
            indentationLevel += 1
            appendLine(builder, "return;")
            indentationLevel -= 1
            appendLine(builder, "end if;")
            builder.append(nextLabel() + "\n")

          case AuthAny(collectionName) =>
            appendLine(builder, s"if sender \\notin $collectionName then")
            indentationLevel += 1
            appendLine(builder, "return;")
            indentationLevel -= 1
            appendLine(builder, "end if;")
            builder.append(nextLabel() + "\n")

          case AuthAll(collectionName) =>
            appendLine(builder, s"${writeApprovalVarRef(transition, subTerms.head)} := TRUE;")
            val paramsRepr = "sender |-> s" +: transition.parameters.getOrElse(Seq.empty[Variable]).
              map(p => s"${p.name} |-> ${p.name}")
            appendLine(builder, s"if ~(\\A s \\in $collectionName: " +
              writeApprovalVarName(transition, subTerms.head) + s"[${paramsRepr.mkString(", ")}])" + " then")
            indentationLevel += 1
            appendLine(builder, "return;")
            indentationLevel -= 1
            appendLine(builder, "end if;")
            builder.append(nextLabel() + "\n")
        }
      } else {
        subTerms.zipWithIndex.foreach { case (subTerm, i) =>
          val conditional = if (i == 0) "if" else "elsif"
          subTerm match {
            case IdentityLiteral(identity) =>
              appendLine(builder, s"$conditional sender = $identity then")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} := TRUE;")
              indentationLevel -= 1

            case AuthAny(collectionName) =>
              appendLine(builder, s"$conditional sender \\in $collectionName then")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} := TRUE;")
              indentationLevel -= 1

            case AuthAll(collectionName) =>
              appendLine(builder, s"$conditional sender \\in $collectionName then")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} := TRUE;")
              indentationLevel -= 1
          }
        }
        appendLine(builder, "end if;")
        builder.append(nextLabel() + "\n")

        appendLine(builder, s"if ~(${writeAuthClause(transition, authTerm)}) then")
        indentationLevel += 1
        appendLine(builder, "return;")
        indentationLevel -= 1
        appendLine(builder, "end if;")
        builder.append(nextLabel() + "\n")
      }
    }

    if (transition.origin.getOrElse("") != transition.destination) {
      appendLine(builder, s"$CURRENT_STATE_VAR := ${transition.destination.toUpperCase};")
    }
    // Add incoming funds to token balance
    if (transition.parameters.getOrElse(Seq.empty[Variable]).contains(Variable("tokens", Int))) {
      appendLine(builder, "balance := balance + tokens;")
    }
    transition.body.foreach(_.foreach { statement =>
      // Statements handle their own indentation
      builder.append(writeStatement(statement))
    })

    if (transition.origin.fold(false)(_ == transition.destination)) {
      builder.append(writeClearAuthTerms(transition))
    }

    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.append("\n")
    builder.toString()
  }

  private def writeTransitionArgumentSelection(parameters: Seq[Variable]): String =
  // Add "_arg" suffix to avoid name collisions in the TLA+ that gets produced from this PlusCal
    "with " + parameters.map(p => s"${p.name + "_arg"} \\in ${writeDomain(p.ty)}").mkString(", ") + " do"

  // Systematically rename each transition's parameter to avoid collisions and facilitate expressive LTL
  // Also means we may need to modify statements in the transition's body
  // Each parameter name now includes the transition's name as a prefix
  private def mangleTransition(transition: Transition, fieldNames: Set[String]): Transition = {
    val newParameters = transition.parameters.map(_.map(p => Variable(s"${transition.name}_${p.name}", p.ty)))
    val newGuard = transition.guard.map(mangleExpression(_, transition.name, fieldNames))
    val newBody = transition.body.map(_.map(mangleStatement(_, transition.name, fieldNames)))

    Transition(transition.name, transition.origin, transition.destination, newParameters,
                transition.authorized, transition.auto, newGuard, newBody)
  }

  // Helper method for mangleTransition
  private def mangleStatement(s: Statement, transName: String, fieldNames: Set[String]): Statement = s match {
    case Assignment(left, right) =>
      Assignment(mangleAssignable(left, transName, fieldNames), mangleExpression(right, transName, fieldNames))
    case Send(destination, amount, source) =>
      Send(mangleExpression(destination, transName, fieldNames), mangleExpression(amount, transName, fieldNames),
           source.map(mangleAssignable(_, transName, fieldNames)))
    case SequenceAppend(sequence, element) =>
      SequenceAppend(mangleExpression(sequence, transName, fieldNames), mangleExpression(element, transName, fieldNames))
    case SequenceClear(sequence) => SequenceClear(mangleExpression(sequence, transName, fieldNames))
  }

  // Helper method for mangleTransition
  private def mangleExpression(e: Expression, transName: String, fieldNames: Set[String]): Expression = e match {
    case ArithmeticOperation(left, operator, right) =>
      ArithmeticOperation(mangleExpression(left, transName, fieldNames), operator, mangleExpression(right, transName, fieldNames))
    case LogicalOperation(left, operator, right) =>
      LogicalOperation(mangleExpression(left, transName, fieldNames), operator, mangleExpression(right, transName, fieldNames))
    case SequenceSize(sequence) => SequenceSize(mangleExpression(sequence,transName, fieldNames))
    case a: Assignable => mangleAssignable(a, transName, fieldNames)
    case _ => e
  }

  private def mangleAssignable(a: Assignable, transName: String, fieldNames: Set[String]): Assignable = a match {
    case MappingRef(map, key) =>
      MappingRef(mangleExpression(map, transName, fieldNames), mangleExpression(key, transName, fieldNames))
    case ScopedParamRef(_,_) => throw new IllegalArgumentException("Found ScopedParamRef in transition body")
    case VarRef(name) => if ((fieldNames contains name) || (RESERVED_NAME_TRANSLATIONS contains name)) {
      VarRef(name)
    } else {
      VarRef(transName + "_" + name)
    }
  }

  // TODO code cleanup
  private def writeInvocationLoop(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    appendLine(builder, "procedure invokeContract(sender) begin InvokeContract:")
    indentationLevel += 1

    // If at root of call tree, stash contract's state to be restored  if exception is thrown
    appendLine(builder, s"if $CALL_DEPTH_VAR = 0 then")
    indentationLevel += 1
    appendLine(builder, s"$STATE_STASH_VAR := [")
    indentationLevel += 1
    val stashedVarNames = getStashedVariables(stateMachine)
    stashedVarNames.dropRight(1).foreach { varName =>
      appendLine(builder, s"$varName |-> $varName,")
    }
    appendLine(builder, s"${stashedVarNames.last} |-> ${stashedVarNames.last}")
    indentationLevel -= 1
    appendLine(builder, "];")
    indentationLevel -= 1
    appendLine(builder, "end if;")

    appendLine(builder, s"$CALL_DEPTH_VAR := $CALL_DEPTH_VAR + 1;")
    appendLine(builder, "with __timeDelta \\in 1..MAX_TIMESTEP do")
    indentationLevel += 1
    appendLine(builder, s"$CURRENT_TIME_VAR := $CURRENT_TIME_VAR + __timeDelta;")
    indentationLevel -= 1
    appendLine(builder, "end with;")
    builder.append("MethodCall:\n")
    appendLine(builder, "either")
    indentationLevel += 1
    val nonInitialTransitions = stateMachine.transitions.filter(_.origin.isDefined)
    nonInitialTransitions.foreach { transition =>
      transition.parameters.foreach { params =>
        appendLine(builder, writeTransitionArgumentSelection(params))
        indentationLevel += 1
      }

      // Again, add "_arg" suffix to avoid name collisions in TLA+
      val paramsRepr = ("sender" +: transition.parameters.getOrElse(Seq.empty[Variable]).map(_.name + "_arg")).mkString(", ")
      appendLine(builder, s"call ${transition.name}($paramsRepr);")
      if (transition.parameters.isDefined) {
        indentationLevel -= 1
        appendLine(builder, "end with;")
      }

      indentationLevel -= 1
      appendLine(builder, "or")
      indentationLevel += 1
    }

    appendLine(builder, "call throw();")
    indentationLevel -= 1
    appendLine(builder, "end either;")
    builder.append(nextLabel() + "\n")
    appendLine(builder, s"$CALL_DEPTH_VAR := $CALL_DEPTH_VAR - 1;")
    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.toString()
  }

  private def writeSendFunction(): String = {
    val builder = new StringBuilder()
    appendLine(builder, "procedure sendTokens(recipient, amount) begin SendTokens:")
    indentationLevel += 1
    appendLine(builder, s"$CONTRACT_BALANCE_VAR := $CONTRACT_BALANCE_VAR - amount;")
    builder.append("SendInvocation:\n")
    appendLine(builder, "either")
    indentationLevel += 1
    appendLine(builder, "call invokeContract(recipient);")
    appendLine(builder, "goto SendInvocation;")
    indentationLevel -= 1
    appendLine(builder, "or")
    indentationLevel += 1
    appendLine(builder, "skip;")
    indentationLevel -= 1
    appendLine(builder, "end either;")
    builder.append(nextLabel() + "\n")
    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.toString()
  }

  private def writeThrowFunction(stateMachine: StateMachine): String = {
    val builder = new StringBuilder()
    appendLine(builder, "procedure throw() begin Throw:")
    indentationLevel += 1
    getStashedVariables(stateMachine).foreach { name =>
      appendLine(builder, name + " := " + STATE_STASH_VAR + "[\"" + name + "\"];")
    }
    appendLine(builder, s"$CALL_DEPTH_VAR := 0;")
    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.toString()
  }

  def writeSpecification(specification: Specification): String = specification match {
    case Specification(name, StateMachine(fields, transitions), _) =>
      val stateMachine = StateMachine(fields, transitions.map(mangleTransition(_, fields.map(_.name).toSet)))
      val initialState = stateMachine.transitions.filter(_.origin.isEmpty).head.destination

      val builder = new StringBuilder()
      appendLine(builder, s"-----MODULE $name-----")
      appendLine(builder, "EXTENDS Integers, Sequences, TLC")

      val stateNames = stateMachine.states.map(_.toUpperCase)
      appendLine(builder, s"CONSTANTS ${BUILT_IN_CONSTANTS.mkString(", ")}")
      appendLine(builder, s"CONSTANTS ${stateNames.mkString(", ")}")
      appendLine(builder, s"STATES == { ${stateMachine.states.map(_.toUpperCase).mkString(", ")} }")

      val allIdentities = "ZERO_IDENT" +: 1.to(TLA.NUM_IDENTITIES).map(n => s"__ident$n")
      appendLine(builder, s"CONSTANTS ${allIdentities.mkString(", ")}")
      appendLine(builder, s"IDENTITIES == { ${allIdentities.mkString(", ")} }")
      builder.append("\n")

      appendLine(builder, s"(* --fair algorithm $name")
      appendLine(builder, s"variables $CURRENT_STATE_VAR = ${initialState.toUpperCase};")
      indentationLevel += 1
      appendLine(builder, s"$CURRENT_TIME_VAR = 0;")
      appendLine(builder, s"$CALL_DEPTH_VAR = 0;")
      appendLine(builder, s"$CONTRACT_BALANCE_VAR = 0;")
      appendLine(builder, "__temporary = 0;")
      builder.append(writeAuthorizationFields(stateMachine))

      stateMachine.fields.foreach { field =>
        appendLine(builder, s"${writeField(field)};")
      }

      builder.append(writeStateStash(stateMachine))
      builder.append("\n")

      val initialTransition = stateMachine.transitions.filter(_.origin.isEmpty).head
      val standardTransitions = stateMachine.transitions.filter(_.origin.isDefined)
      val autoTransitions = stateMachine.transitions.filter(_.auto).foldLeft(Map.empty[String, Seq[Transition]]) { (autoTrans, transition) =>
        val originState = transition.origin.get
        autoTrans + (originState -> (autoTrans.getOrElse(originState, Seq.empty[Transition]) :+ transition))
      }
      builder.append(writeTransition(initialTransition, autoTransitions))
      standardTransitions.foreach(t => builder.append(writeTransition(t, autoTransitions)))

      builder.append(writeInvocationLoop(stateMachine))
      builder.append("\n")
      builder.append(writeSendFunction())
      builder.append("\n")
      builder.append(writeThrowFunction(stateMachine))
      builder.append("\n")

      // Invoke procedure corresponding to initial transition
      builder.append("begin Main:\n")
      // Add the usual "_arg" suffix to prevent name collisions in TLA+
      val initialParams = Variable("sender", Identity) +: initialTransition.parameters.getOrElse(Seq.empty[Variable])
      val initialParamsChoice = initialParams.map(p => s"${p.name}_arg \\in ${writeDomain(p.ty)}").mkString(", ")
      appendLine(builder, s"with $initialParamsChoice do")
      indentationLevel += 1
      appendLine(builder, s"call ${initialTransition.name}(${initialParams.map(_.name + "_arg").mkString(", ")});")
      indentationLevel -= 1
      appendLine(builder, "end with;")
      builder.append("\n")

      builder.append("Loop:\n")
      appendLine(builder, s"with sender_arg \\in ${writeDomain(Identity)} do")
      indentationLevel += 1
      appendLine(builder, "call invokeContract(sender_arg);")
      indentationLevel -= 1
      appendLine(builder, "end with;")
      builder.append(nextLabel() + "\n")
      appendLine(builder, "goto Loop;")

      indentationLevel -= 1
      appendLine(builder, "end algorithm; *)")
      appendLine(builder, "========")
      builder.toString()
  }
}
