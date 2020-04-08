package edu.berkeley.cs.rise.bcdsl

case class Variable(name: String, ty: DataType)

case class Transition(name: String, origin: Option[String], destination: String, parameters: Option[Seq[Variable]],
                      authorized: Option[AuthExpression], auto: Boolean, guard: Option[Expression], body: Option[Seq[Statement]]) {
  val description: String = s"$name: '${origin.getOrElse("")}' -> '$destination'"

  private def makeTypeErrMsg(idx: scala.Int, msg: String): String =
    s"Type error on statement ${idx + 1} of transition $description: $msg"

  // Assign a unique name to each term in the transition's authorization clause
  // This is useful for things like generating names for variables to track
  // previous authorizations
  val authTermNames: Map[AuthTerm, String] = {
    val terms = authorized.fold(Set.empty[AuthTerm])(_.basis)
    val grouped = terms.groupBy(_.referencedAssignable.rootName)

    grouped.foldLeft(Map.empty[AuthTerm, String]) { case (current, (name, terms)) =>
      current ++ terms.zipWithIndex.map { case (term, idx) => term -> s"${name}_$idx" }
    }
  }

  // There is probably a much nicer way to structure this logic
  def validate(context: Context): Option[String] = {
    val paramsMap = parameters.fold(Map.empty[String, DataType]) { params =>
      params.map(variable => variable.name -> variable.ty).toMap
    }
    // Check that transition parameters do not shadow previously defined variables
    val shadowedParams = paramsMap.keySet.intersect(context.variables.keySet).headOption
    if (shadowedParams.isDefined) {
      return Some(s"Transition $description shadows field or keyword ${shadowedParams.get}")
    }

    // Check that all structs used as parameter types are well defined
    val undefinedStructName = paramsMap.values.collect{ case Struct(name) => name }.find(!context.structs.contains(_))
    if (undefinedStructName.isDefined) {
      return Some(s"Transition $description has parameter of undefined struct type ${undefinedStructName.get}")
    }

    // And check that any constrained parameters are of the correct type
    val constrainedNames = paramsMap.keySet.intersect(Specification.CONSTRAINED_PARAMS.keySet)
    val nameError = constrainedNames.flatMap { name =>
      val actualTy = paramsMap(name)
      val expectedTy = Specification.CONSTRAINED_PARAMS(name)
      if (actualTy != expectedTy) {
        Some(s"Special parameter $name is of type $actualTy when it must be of type $expectedTy")
      } else {
        None
      }
    }.headOption
    if (nameError.isDefined) {
      return nameError
    }

    // Check that transition is not designated as automatic but has an authorization restriction
    if (auto && authorized.isDefined) {
      return Some(s"Automatic transition $description cannot have authorization restriction")
    }
    // Check that transition is not designated as automatic but lacks a guard
    if (auto && guard.isEmpty) {
      return Some(s"Automatic transition $description lacks a guard")
    }

    val localContext = Context(context.structs, context.variables ++ paramsMap)

    // Check that transition guard is correctly typed
    guard.fold(Right(Bool): Either[String, DataType])(_.getType(localContext)) match {
      case Left(err) => return Some(s"Invalid guard in transition $description: $err")
      case Right(Bool) => ()
      case Right(ty) => return Some(s"Guard in transition $description is of non-boolean type $ty")
    }

    // Check that authorization clause does not reference undefined identities or invalid types
    val authErr = authorized.fold(None: Option[String])(_.validate(context))
    if (authErr.isDefined) {
      return authErr.map(err =>  s"$description: $err")
    }

    // Type check all transition body statements
    body.fold(None: Option[String])(b => b.zipWithIndex.foldLeft(None: Option[String]) { case (prev, (current, i)) =>
      prev match {
        case s@Some(_) => s
        case None => current.validate(localContext).map(makeTypeErrMsg(i, _))
      }
    })
  }

  def flattenExpressions(): Seq[Expression] = {
    val bodyExpressions = body.fold(Seq.empty[Expression])(_.flatMap {
      case Assignment(left, right) => Seq(left, right)
      case SequenceAppend(sequence, element) => Seq(sequence, element)
      case SequenceClear(sequence) => Seq(sequence)
      case Send(dest, amount, Some(source)) => Seq(dest, amount, source)
      case Send(dest, amount, None) => Seq(dest, amount)
    })
    guard.fold(bodyExpressions)(_ +: bodyExpressions)
  }
}

case class StateMachine(structs: Map[String, Map[String, DataType]], fields: Seq[Variable], transitions: Seq[Transition]) {
  val states: Set[String] = transitions.foldLeft(Set.empty[String]) { (currentStates, transition) =>
    currentStates ++ transition.origin.toSet + transition.destination
  }

  def validate(): Option[String] = {
    // Check that no fields shadow a reserved value
    val shadowField = fields.map(_.name).toSet.intersect(Specification.RESERVED_VALUES.keySet).headOption
    if (shadowField.isDefined) {
      return Some(s"State machine field ${shadowField.get} shadows reserved value")
    }

    // Check that all struct types used in fields and transitions are defined
    // Check that all structs used as parameter types are well defined
    val undefinedStructName = fields.map(_.ty).collect { case Struct(name) => name }.find(!structs.contains(_))
    if (undefinedStructName.isDefined) {
      return Some(s"State machine has field of undefined struct type ${undefinedStructName.get}")
    }

    // Check that only one transition is missing a source, this indicates the initial state
    val initialTransitions = transitions.filter(_.origin.isEmpty)
    if (initialTransitions.isEmpty) {
      return Some("State machine is missing an initial transition")
    } else if (initialTransitions.length > 1) {
      return Some("State machine has multiple initial transitions")
    }

    // The initial transition cannot have an authorization requirement
    if (initialTransitions.head.authorized.isDefined) {
      return Some("Initial transition cannot have an authorization restriction")
    }
    // Nor can it be designated as an auto transition
    if (initialTransitions.head.auto) {
      return Some("Initial transition cannot be designated as automatic")
    }

    val initialState = initialTransitions.head.destination
    val allStates = transitions.foldLeft(Set.empty[String]) { (states, transition) =>
      transition.origin match {
        case Some(o) => states + (o, transition.destination)
        case None => states + transition.destination
      }
    }
    // Check that all states are reachable from initial state
    val adjacencyList = StateMachine.transitionsToAdjList(transitions)
    val simpleAdjacencyList = adjacencyList.mapValues(_.map(_.destination))
    val reachableStates = StateMachine.dfsVisit(initialState, simpleAdjacencyList, Set.empty[String])
    val unreachableStates = allStates.diff(reachableStates)
    if (unreachableStates.nonEmpty) {
      return Some("Unreachable states: " + unreachableStates.mkString(", "))
    }

    // Finally, validate all transitions
    val varContext = fields.foldLeft(Specification.RESERVED_VALUES) { (ctx, field) =>
      ctx + (field.name -> field.ty)
    }
    val context = Context(structs, varContext)
    transitions.flatMap(_.validate(context)).headOption
  }

  def flattenExpressions: Seq[Expression] = transitions.flatMap(_.flattenExpressions())

  def flattenStatements: Seq[Statement] = transitions.flatMap(_.body.getOrElse(Seq.empty[Statement]))
}

object StateMachine {
  private def transitionsToAdjList(transitions: Seq[Transition]): Map[String, Set[Transition]] =
    transitions.foldLeft(Map.empty[String, Set[Transition]]) { (adjList, transition) =>
      transition.origin.foldLeft(adjList) { (m, o) => m + (o -> (m.getOrElse(o, Set.empty[Transition]) + transition))
      }
    }

  private def dfsVisit(state: String, adjacencyList: Map[String, Set[String]], visited: Set[String]): Set[String] =
    adjacencyList.get(state) match {
      case None => visited + state
      case Some(neighbors) =>
        val newVisited = visited + state
        neighbors.diff(newVisited).foldLeft(newVisited) { (currentVisited, neighbor) =>
          currentVisited ++ dfsVisit(neighbor, adjacencyList, currentVisited)
        }
    }
}
