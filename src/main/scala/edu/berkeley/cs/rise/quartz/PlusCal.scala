package edu.berkeley.cs.rise.quartz

object PlusCal {

  private val INDENTATION_STR = "    "
  private val BUILT_IN_CONSTANTS = Seq("MAX_INT, MIN_INT, MAX_TIMESTEP", "MAX_ELAPSED_TIME", "MAX_CALL_DEPTH")
  private val CONTRACT_BALANCE_VAR = "balance"
  private[quartz] val CALL_DEPTH_VAR = "__contractCallDepth"
  private[quartz] val CURRENT_TIME_VAR = "__currentTime"
  private val CURRENT_STATE_VAR = "__currentState"
  private val STATE_STASH_VAR = "__stateStash"

  private type StructRegistry = Map[String, Map[String, DataType]]

  private var labelCounter = 0
  private var indentationLevel = 0

  private val RESERVED_NAME_TRANSLATIONS: Map[String, String] = Map[String, String](
    "balance" -> "balance",
    "sender" -> "sender",
    "now" -> "__currentTime",
  )

  private val TLA_SUM_OPERATORS =
    """
      |RECURSIVE SeqSum(_)
      |SeqSum(S) == IF S = <<>> THEN 0 ELSE Head(S) + SeqSum(Tail(S))
      |
      |RECURSIVE MapSum(_)
      |MapSum(M) == IF DOMAIN M = {} THEN 0 ELSE
      |             LET x == CHOOSE x \in DOMAIN M: TRUE
      |             IN M[x] + MapSum([ y \in  (DOMAIN M \ {x}) |-> M[y] ])
      |""".stripMargin

  private def appendLine(builder: StringBuilder, line: String): Unit =
    builder.append(s"${INDENTATION_STR * indentationLevel}$line\n")

  private def writeLine(line: String): String = s"${INDENTATION_STR * indentationLevel}$line\n"

  private def writeDomain(ty: DataType, structs: StructRegistry): String = ty match {
    case Identity => "IDENTITIES \\ {ZERO_IDENT}"
    case IntVar => "MIN_INT..MAX_INT"
    case IntConst => throw new UnsupportedOperationException("Write domain of int constant")
    case UnsignedIntVar => "0..MAX_INT"
    case UnsignedIntConst => throw new UnsupportedOperationException("Write domain of unsigned int constant")
    case Bool => "{ TRUE, FALSE }"
    case Timestamp => "0..MAX_INT"
    case Timespan => "0..MAX_INT"
    case String => (1 to TLA.NUM_STRINGS).map(i => "\"" + s"str_${i.toString}" + "\"").mkString("{", ",", "}")
    case HashValue(payloadTypes) => payloadTypes match {
      case Seq(t) => s"[ {1} -> (${writeDomain(t, structs)}) ]"
      case _ => payloadTypes.map(t => s"(${writeDomain(t, structs)})").mkString("(", " \\X ", ")")
    }
    case Mapping(keyType, valueType) => s"[ ${writeDomain(keyType, structs)} -> ${writeDomain(valueType, structs)} ]"
    case Struct(name) => "[" + structs(name).map { case (name, ty) => s"$name: ${writeDomain(ty, structs)}" }.mkString(", ") + "]"
    case Sequence(elementType) => s"[ x \\in 1..MAX_INT -> ${writeDomain(elementType, structs)} ]"
  }

  private def writeZeroElement(ty: DataType, structs: StructRegistry): String = ty match {
    case Identity => "ZERO_IDENT"
    case IntVar => "0"
    case IntConst => throw new UnsupportedOperationException("Write zero value of int constant")
    case UnsignedIntVar => "0"
    case UnsignedIntConst => throw new UnsupportedOperationException("Write zero value of unsigned int const")
    case String => "\"\""
    case Timestamp => "0"
    case Bool => "FALSE"
    case Timespan => "0"
    case HashValue(payloadTypes) => payloadTypes.map(t => writeZeroElement(t, structs)).mkString("<<", ", ", ">>")
    case Mapping(keyType, valueType) => s"[ x \\in ${writeDomain(keyType, structs)} |-> ${writeZeroElement(valueType, structs)} ]"
    case Sequence(_) => "<<>>"
    case Struct(name) =>
      "[" + structs(name).map { case (name, ty) => s"$name |-> ${writeZeroElement(ty, structs)}" }.mkString(", ") + "]"
  }

  private def writeField(field: Variable, structs: StructRegistry): String = s"${field.name} = ${writeZeroElement(field.ty, structs)}"

  private def writeMaxVarName(assignable: Assignable): String = s"__max_${TLA.flattenName(assignable)}"

  private def writeMinVarName(assignable: Assignable): String=  s"__min_${TLA.flattenName(assignable)}"

  private def writeApprovalVarName(transition: Transition, term: AuthTerm): String =
  // Only non-initial transitions can have authorization clause
    s"${transition.name}_${transition.authTermNames(term)}_approved"

  private def writeApprovalVarRef(transition: Transition, term: AuthTerm): String = term match {
    case IdentityRef(_) | AuthAny(_) => transition.parameters match {
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

  private def writeApprovalVarInit(transition: Transition, term: AuthTerm, structs: StructRegistry): String = transition.parameters match {
    case None => "FALSE"
    case Some(params) =>
      val effectiveParams = term match {
        case IdentityRef(_) | AuthAny(_) => params
        case AuthAll(_) => Variable(RESERVED_NAME_TRANSLATIONS("sender"), Identity) +: params
      }
      val paramsStructRepr = "[" + effectiveParams.map(p => s"${p.name}: ${writeDomain(p.ty, structs)}").mkString(", ") + "]"
      s"[ x \\in $paramsStructRepr |-> FALSE]"
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
            appendLine(builder, s"${writeApprovalVarName(trans, terms.head)} = ${writeApprovalVarInit(trans, terms.head, machine.structs)};")
        }
      } else {
        terms.foreach { term =>
          appendLine(builder, s"${writeApprovalVarName(trans, term)} = ${writeApprovalVarInit(trans, term, machine.structs)};")
        }
      }
    })

    builder.toString()
  }

  private def getAuthorizationFieldNames(machine: StateMachine): Seq[String] = machine.transitions.flatMap { transition =>
    transition.authorized.fold(Seq.empty[String]) { authClause =>
      val terms = authClause.basis
      if (terms.size == 1) {
        terms.head match {
          case IdentityRef(_) | AuthAny(_) => Seq.empty[String]
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

  private def writeAuthClause(transition: Transition, term: AuthExpression, depth: scala.Int = 0): String = {
    val builder = new StringBuilder()
    term match {
      case t: AuthTerm => t match {
        case IdentityRef(_) | AuthAny(_) => builder.append(writeApprovalVarRef(transition, t))

        case AuthAll(collection) =>
          val paramReprs = "sender |-> s" +: transition.parameters.getOrElse(Seq.empty[Variable]).map(p => s"${p.name} |-> ${p.name}")
          builder.append(s"\\A s \\in ${writeExpression(collection)}: " +
            s"${writeApprovalVarName(transition, t)}[${paramReprs.mkString(", ")}]")
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
          case Implies => builder.append(" => ")
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
      writeLine(s"${writeApprovalVarRef(transition, term)} := FALSE;")
    case AuthAll(collection) =>
      val varName = writeApprovalVarName(transition, term)
      val paramReprs = transition.parameters.fold(Seq.empty[String])(_.map(p => s"${p.name} |-> ${p.name}"))
      val effectiveParamReprs = s"sender \\in ${writeExpression(collection)}" +: paramReprs
      writeLine(s"$varName[${effectiveParamReprs.mkString(", ")}] := FALSE;")
  }

  private def writeExpression(expression: Expression): String =
    expression match {
      case VarRef(name) => RESERVED_NAME_TRANSLATIONS.getOrElse(name, name)
      case MappingRef(map, key) => s"${writeExpression(map)}[${writeExpression(key)}]"
      case StructAccess(struct, field) => s"${writeExpression(struct)}.$field"
      case IntConst(v) => v.toString
      case UnsignedIntConst(v) => v.toString
      case StringLiteral(s) => "\"" + s + "\""
      case BoolConst(b) => b.toString.toUpperCase
      case Hash(payload) => payload.map(writeExpression).mkString("<<", ", ", ">>")
      case SequenceSize(sequence) => s"Len(${writeExpression(sequence)})"
      case Second => "1"
      case Minute => "60"
      case Hour => "3600"
      case Day => "86400"
      case Week => "604800"
      case LTLMax(body) => writeMaxVarName(body)
      case LTLMin(body) => writeMinVarName(body)
      case LTLSum(_) => throw new NotImplementedError("LTLSum")

      case ArithmeticOperation(left, op, right) =>
        val builder = new StringBuilder()
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
          case Modulo => builder.append(" % ")
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(right))
        }

        builder.toString()

      case LogicalOperation(element, op@(In | NotIn), sequence) =>
        val builder = new StringBuilder()
        if (op == NotIn) {
          builder.append("~(")
        }
        builder.append(s"\\E x \\in DOMAIN ${writeExpression(sequence)}: ")
        builder.append(s"${writeExpression(sequence)}[x] = ${writeExpression(element)}")
        if (op == NotIn) {
          builder.append(")")
        }
        builder.toString()

      case LogicalOperation(left, op, right) =>
        val builder = new StringBuilder()
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
          case Implies => builder.append(" => ")
          case In | NotIn => throw new IllegalArgumentException // This should never be reached
        }

        right match {
          case ArithmeticOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case LogicalOperation(_, _, _) => builder.append(s"(${writeExpression(right)})")
          case _ => builder.append(writeExpression(right))
        }

        builder.toString()
  }

  private def writeStatement(statement: Statement, lineTerminator: String): String = statement match {
    case Assignment(left, right) => writeLine(s"${writeExpression(left)} := ${writeExpression(right)}$lineTerminator")

    case SequenceAppend(sequence, element) =>
      writeLine(s"${writeExpression(sequence)} := Append(${writeExpression(sequence)}, ${writeExpression(element)})$lineTerminator")

    case SequenceClear(sequence) =>
      writeLine(s"${writeExpression(sequence)} := <<>>$lineTerminator")

    case Send(destination, amount, source) => source match {
      case None => writeLine(s"call sendTokens(${writeExpression(destination)}, ${writeExpression(amount)})$lineTerminator")
      case Some(s) =>
        val builder = new StringBuilder()
        appendLine(builder, s"__temporary := ${writeExpression(amount)};")
        appendLine(builder, s"${writeExpression(s)} := ${writeExpression(s)} - __temporary;")
        appendLine(builder, s"call sendTokens(${writeExpression(destination)}, __temporary)$lineTerminator")
        builder.toString()
    }

    case Conditional(condition, ifArm, elseArm) =>
      val builder = new StringBuilder()
      appendLine(builder, s"if ${writeExpression(condition)} then")
      indentationLevel += 1
      builder.append(writeStatementSequence(ifArm))
      indentationLevel -= 1
      elseArm.foreach{ stmts =>
        appendLine(builder, "else")
        indentationLevel += 1
        builder.append(writeStatementSequence(stmts))
        indentationLevel -= 1
      }
      appendLine(builder, s"end if$lineTerminator")

      builder.toString()
  }

  private def nextLabel(): String = {
    val currentLabelIndex = labelCounter
    labelCounter += 1
    s"L$currentLabelIndex:"
  }

  private def writeTransition(transition: Transition, maxVals: Set[Assignable], minVals: Set[Assignable]): String = {
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
      val subTerms = authTerm.basis
      if (subTerms.size == 1) {
        subTerms.head match {
          case IdentityRef(assignable) =>
            appendLine(builder, s"if sender /= ${writeExpression(assignable)} then")
            indentationLevel += 1
            appendLine(builder, "return;")
            indentationLevel -= 1
            appendLine(builder, "end if;")
            builder.append(nextLabel() + "\n")

          case AuthAny(collection) =>
            appendLine(builder, s"if sender \\notin ${writeExpression(collection)} then")
            indentationLevel += 1
            appendLine(builder, "return;")
            indentationLevel -= 1
            appendLine(builder, "end if;")
            builder.append(nextLabel() + "\n")

          case AuthAll(collection) =>
            appendLine(builder, s"${writeApprovalVarRef(transition, subTerms.head)} := TRUE;")
            val paramsRepr = "sender |-> s" +: transition.parameters.getOrElse(Seq.empty[Variable]).
              map(p => s"${p.name} |-> ${p.name}")
            appendLine(builder, s"if ~(\\A s \\in ${writeExpression(collection)}: " +
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
            case IdentityRef(assignable) =>
              appendLine(builder, s"$conditional sender = ${writeExpression(assignable)} then")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} := TRUE;")
              indentationLevel -= 1

            case AuthAny(collection) =>
              appendLine(builder, s"$conditional sender \\in ${writeExpression(collection)} then")
              indentationLevel += 1
              appendLine(builder, s"${writeApprovalVarRef(transition, subTerm)} := TRUE;")
              indentationLevel -= 1

            case AuthAll(collection) =>
              appendLine(builder, s"$conditional sender \\in ${writeExpression(collection)} then")
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
    if (transition.parameters.getOrElse(Seq.empty[Variable]).contains(Variable(s"${transition.name}_tokens", UnsignedIntVar))) {
      appendLine(builder, s"balance := balance + ${transition.name}_tokens;")
    }
    builder.append(writeTransitionMaxMinUpdates(transition, maxVals, minVals))
    transition.body.foreach(b => builder.append(writeStatementSequence(b)))

    if (transition.origin.fold(false)(_ == transition.destination)) {
      // Add a label to separate authorization updates from body statements, if necessary
      transition.body.foreach(_ => builder.append(nextLabel() + "\n"))
      builder.append(writeClearAuthTerms(transition))
    }

    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.append("\n")
    builder.toString()
  }

  private def writeTransitionArgumentSelection(parameters: Seq[Variable], structs: StructRegistry): String =
  // Add "_arg" suffix to avoid name collisions in the TLA+ that gets produced from this PlusCal
    "with " + parameters.map(p => s"${p.name + "_arg"} \\in ${writeDomain(p.ty, structs)}").mkString(", ") + " do"

  private def writeStatementSequence(body: Seq[Statement]): String = body match {
    case Seq(s) => writeStatement(s, ";")
    case _ =>
      val builder = new StringBuilder()
      // Walk through statements and use "||" as terminator to denote simultaneous assignment
      // Until we find a variable previously assigned to or a send or a conditional
      // Then use ";" terminator and reset knowledge of previous assignment targets
      body.sliding(2).foldLeft(Set.empty[Assignable]) { case (prevAssignments, Seq(current, next)) =>
        current match {
          case _: Send | _: Conditional =>
            writeStatement(current, s";\n${nextLabel()}\n")
            Set.empty[Assignable]

          case _ =>
            val currentAssignments = current match {
              case Assignment(left, _) => prevAssignments + left
              case SequenceAppend(sequence, _) => prevAssignments + sequence
              case SequenceClear(sequence) => prevAssignments + sequence
              case _: Conditional | _: Send => throw new RuntimeException // Unreachable
            }

            val (terminator, nextAssignments) = next match {
              case Assignment(left, _) => if (currentAssignments.contains(left)) {
                (s";\n${nextLabel()}", Set.empty[Assignable])
              } else {
                (" ||", currentAssignments)
              }

              case SequenceAppend(sequence, _) => if (currentAssignments.contains(sequence)) {
                (s";\n${nextLabel()}", Set.empty[Assignable])
              } else {
                (" ||", currentAssignments)
              }

              case SequenceClear(sequence) => if (currentAssignments.contains(sequence)) {
                (s";\n${nextLabel()}", Set.empty[Assignable])
              } else {
                (" ||", currentAssignments)
              }

              case _: Send | _: Conditional => (s";${nextLabel()}", Set.empty[Assignable])
            }

            builder.append(writeStatement(current, terminator))
            nextAssignments
        }
      }

      // And don't forget the last statement!
      builder.append(writeStatement(body.last, ";"))
      builder.toString()
  }

  private def writeTransitionMaxMinUpdates(transition: Transition, maxVals: Set[Assignable], minVals: Set[Assignable]): String = {
    val builder = new StringBuilder()

    val paramNames = transition.parameters.fold(Set.empty[String])(_.map(_.name).toSet)
    val effectiveMaxVals = maxVals.map {
      case assignable@StructAccess(struct, fieldName) => struct match {
        case VarRef(structName) => if (paramNames.contains(s"${structName}_$fieldName")) {
          VarRef(s"${structName}_$fieldName")
        } else {
          assignable
        }

        case _ => assignable
      }
      case a => a
    }

    val effectiveMinVals = minVals.map {
      case assignable@StructAccess(struct, fieldName) => struct match {
        case VarRef(structName) => if (paramNames.contains(s"${structName}_$fieldName")) {
          VarRef(s"${structName}_$fieldName")
        } else {
          assignable
        }

        case _ => assignable
      }

      case a => a
    }
    val paramVals = transition.parameters.fold(Seq.empty[Assignable])(_.map(p => VarRef(p.name)))

    // Update any necessary maxVals or minVals
    val modifiedVals = transition.body.fold(Seq.empty[Assignable])(_.collect { case Assignment(left, _) => left })
    (paramVals ++ modifiedVals).foreach { assignable =>
      if (effectiveMaxVals.contains(assignable)) {
        appendLine(builder,s"if ${writeExpression(assignable)} > ${writeMaxVarName(assignable)} then")
        indentationLevel += 1
        appendLine(builder, s"${writeMaxVarName(assignable)} := ${writeExpression(assignable)}")
        indentationLevel -= 1
        appendLine(builder, "end if;")
      } else if (effectiveMinVals.contains(assignable)) {
        appendLine(builder, s"if ${writeExpression(assignable)} < ${writeMinVarName(assignable)} then")
        indentationLevel += 1
        appendLine(builder, s"${writeMinVarName(assignable)} := ${writeExpression(assignable)}")
        indentationLevel -= 1
        appendLine(builder, "end if;")
      }
    }

    builder.toString()
  }

  // Systematically rename each transition's parameters to avoid collisions and facilitate expressive LTL
  // Also means we may need to modify statements in the transition's body
  // Each parameter name now includes the transition's name as a prefix
  private def mangleTransition(transition: Transition): Transition = {
    val newParameters = transition.parameters.map(_.map(p => Variable(s"${transition.name}_${p.name}", p.ty)))
    val newGuard = transition.guard.map(mangleExpression(_, transition))
    val newBody = transition.body.map(_.map(mangleStatement(_, transition)))

    Transition(transition.name, transition.origin, transition.destination, newParameters,
      transition.authorized, newGuard, newBody)
  }

  // Helper method for mangleTransition
  private def mangleStatement(s: Statement, transition: Transition): Statement = s match {
    case Assignment(left, right) =>
      Assignment(mangleAssignable(left, transition), mangleExpression(right, transition))
    case Send(destination, amount, source) =>
      Send(mangleExpression(destination, transition), mangleExpression(amount, transition),
        source.map(mangleAssignable(_, transition)))
    case SequenceAppend(sequence, element) =>
      SequenceAppend(mangleAssignable(sequence, transition), mangleExpression(element, transition))
    case SequenceClear(sequence) => SequenceClear(mangleAssignable(sequence, transition))
    case Conditional(condition, ifArm, elseArm) =>
      val mangledCondition = mangleExpression(condition, transition)
      val mangledIf = ifArm.map(s => mangleStatement(s, transition))
      val mangledElse = elseArm.map(_.map(s => mangleStatement(s, transition)))
      Conditional(mangledCondition, mangledIf, mangledElse)
  }

  // Helper method for mangleTransition
  private def mangleExpression(e: Expression, transition: Transition): Expression = e match {
    case ArithmeticOperation(left, operator, right) =>
      ArithmeticOperation(mangleExpression(left, transition), operator, mangleExpression(right, transition))
    case LogicalOperation(left, operator, right) =>
      LogicalOperation(mangleExpression(left, transition), operator, mangleExpression(right, transition))
    case SequenceSize(sequence) => SequenceSize(mangleExpression(sequence, transition))
    case Hash(payload) => Hash(payload.map(mangleExpression(_, transition)))
    case a: Assignable => mangleAssignable(a, transition)
    case _ => e
  }

  private def mangleAssignable(a: Assignable, transition: Transition): Assignable = {
    val parameterNames = transition.parameters.fold(Seq.empty[String])(_.map(_.name))
    a match {
      case v@VarRef(name) => if (parameterNames contains name) {
        VarRef(s"${transition.name}_$name")
      } else {
        v
      }
      case MappingRef(map, key) =>
        MappingRef(mangleAssignable(map, transition), mangleExpression(key, transition))
      case StructAccess(struct, field) =>
        StructAccess(mangleAssignable(struct, transition), field)
    }
  }

  // TODO code cleanup
  private def writeInvocationLoop(stateMachine: StateMachine, useCall: Boolean = false): String = {
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
    appendLine(builder, s"if $CALL_DEPTH_VAR < 2 then")
    indentationLevel += 1
    appendLine(builder, "with __timeDelta \\in 1..MAX_TIMESTEP do")
    indentationLevel += 1
    appendLine(builder, s"$CURRENT_TIME_VAR := $CURRENT_TIME_VAR + __timeDelta;")
    indentationLevel -= 1
    appendLine(builder, "end with;")
    indentationLevel -= 1
    appendLine(builder, "end if;")
    builder.append("MethodCall:\n")

    val nonInitialTransitions = stateMachine.transitions.filter(_.origin.isDefined)
    if (nonInitialTransitions.length > 1 || useCall) {
      appendLine(builder, "either")
      indentationLevel += 1
    }
    nonInitialTransitions.zipWithIndex.foreach { case (transition, i) =>
      transition.parameters.foreach { params =>
        appendLine(builder, writeTransitionArgumentSelection(params, stateMachine.structs))
        indentationLevel += 1
      }

      // Again, add "_arg" suffix to avoid name collisions in TLA+
      val paramsRepr = ("sender" +: transition.parameters.getOrElse(Seq.empty[Variable]).map(_.name + "_arg")).mkString(", ")
      appendLine(builder, s"call ${transition.name}($paramsRepr);")
      if (transition.parameters.isDefined) {
        indentationLevel -= 1
        appendLine(builder, "end with;")
      }

      if (i < nonInitialTransitions.length - 1) {
        indentationLevel -= 1
        appendLine(builder, "or")
        indentationLevel += 1
      }
    }

    if (nonInitialTransitions.length > 1 || useCall) {
      indentationLevel -= 1
      appendLine(builder, "end either;")
    }
    builder.append(nextLabel() + "\n")
    appendLine(builder, s"$CALL_DEPTH_VAR := $CALL_DEPTH_VAR - 1;")
    appendLine(builder, "return;")
    indentationLevel -= 1
    appendLine(builder, "end procedure;")
    builder.toString()
  }

  private def writeSendFunction(useCall: Boolean = false): String = {
    val builder = new StringBuilder()
    appendLine(builder, "procedure sendTokens(recipient, amount) begin SendTokens:")
    indentationLevel += 1
    appendLine(builder, s"$CONTRACT_BALANCE_VAR := $CONTRACT_BALANCE_VAR - amount;")
    builder.append("SendInvocation:\n")
    appendLine(builder, "either")
    indentationLevel += 1
    if (useCall) {
      appendLine(builder, "call invokeContract(recipient);")
      appendLine(builder, "goto SendInvocation;")
    } else {
      appendLine(builder, "call throw();")
    }
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

  def writeSpecification(specification: Specification, useCall: Boolean = false): String = specification match {
    case Specification(name, StateMachine(structs, fields, transitions), invariants) =>
      val (maxVals, minVals) = invariants.fold((Set.empty[Assignable], Set.empty[Assignable]))(
        _.foldLeft((Set.empty[Assignable], Set.empty[Assignable])) { case ((prevMax, prevMin), prop) =>
          val (currentMax, currentMin) = Specification.extractMaxMinTargets(prop)
          (prevMax.union(currentMax), prevMin.union(currentMin))
        }
      )

      val stateMachine = StateMachine(structs, fields, transitions.map(mangleTransition))
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

      builder.append(TLA_SUM_OPERATORS)
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
        appendLine(builder, s"${writeField(field, stateMachine.structs)};")
      }

      maxVals.foreach { assignable =>
        appendLine(builder, s"${writeMaxVarName(assignable)} = MIN_INT;")
      }
      minVals.foreach { assignable =>
        appendLine(builder, s"${writeMinVarName(assignable)} = MAX_INT;")
      }

      builder.append(writeStateStash(stateMachine))
      builder.append("\n")

      val initialTransition = stateMachine.transitions.filter(_.origin.isEmpty).head
      val standardTransitions = stateMachine.transitions.filter(_.origin.isDefined)
      builder.append(writeTransition(initialTransition, maxVals, minVals))
      standardTransitions.foreach(t => builder.append(writeTransition(t, maxVals, minVals)))

      builder.append(writeInvocationLoop(stateMachine, useCall))
      builder.append("\n")
      builder.append(writeSendFunction(useCall))
      builder.append("\n")
      builder.append(writeThrowFunction(stateMachine))
      builder.append("\n")

      // Invoke procedure corresponding to initial transition
      builder.append("begin Main:\n")
      // Add the usual "_arg" suffix to prevent name collisions in TLA+
      val initialParams = Variable("sender", Identity) +: initialTransition.parameters.getOrElse(Seq.empty[Variable])
      val initialParamsChoice = initialParams.map(p => s"${p.name}_arg \\in ${writeDomain(p.ty, stateMachine.structs)}").mkString(", ")
      appendLine(builder, s"with $initialParamsChoice do")
      indentationLevel += 1
      appendLine(builder, s"call ${initialTransition.name}(${initialParams.map(_.name + "_arg").mkString(", ")});")
      indentationLevel -= 1
      appendLine(builder, "end with;")
      builder.append("\n")

      builder.append("Loop:\n")
      appendLine(builder, s"with sender_arg \\in ${writeDomain(Identity, stateMachine.structs)} do")
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
