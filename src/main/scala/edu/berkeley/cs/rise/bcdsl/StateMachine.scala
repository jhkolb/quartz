package edu.berkeley.cs.rise.bcdsl

case class Variable(name: String, ty: DataType)

case class Assignment(leftSide: Assignable, rightSide: Expression)

case class Transition(origin: Option[String], destination: String, parameters: Option[Seq[Variable]],
                      authorized: Option[AuthDecl], auto: Boolean, guard: Option[Expression],
                      body: Option[Seq[Assignment]]) {
  val name: String = s"'${origin.getOrElse("")}' -> '$destination'"
}

case class StateMachine(fields: Seq[Variable], transitions: Seq[Transition]) {
  def validate(): Option[String] = {
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
    val adjacencyList = transitionsToAdjList(transitions)
    val simpleAdjacencyList = adjacencyList.mapValues(_.map(_.destination))
    val reachableStates = dfsVisit(initialState, simpleAdjacencyList, Set.empty[String])
    val unreachableStates = allStates.diff(reachableStates)
    if (unreachableStates.nonEmpty) {
      return Some("Unreachable states: " + unreachableStates.mkString(", "))
    }

    val principals = fields.filter(_.ty == Identity).map(_.name).toSet
    val context = fields.foldLeft(Map.empty[String, DataType]) { (ctx, field) =>
      ctx + (field.name -> field.ty)
    }

    for (transition <- transitions) {
      val localContext = transition.parameters.fold(context)(_.foldLeft(context) { (ctxt, param) =>
        ctxt + (param.name -> param.ty)
      })

      // Check that transition is not designated as automatic but has an authorization restriction
      if (transition.auto && transition.authorized.isDefined) {
        return Some(s"Automatic transition ${transition.name} cannot have authorization restriction")
      }
      // Check that transition is not designated as automatic but lacks a guard
      if (transition.auto && transition.guard.isEmpty) {
        return Some(s"Automatic transition ${transition.name} lacks a guard")
      }

      // Check that transition guard is correctly typed
      for (expr <- transition.guard) {
        expr.getType(localContext) match {
          case Left(err) => return Some(s"Invalid guard in transition ${transition.name}: $err")
          case Right(Bool) => ()
          case Right(ty) => return Some(s"Guard in transition ${transition.name} is of non-boolean type $ty")
        }
      }

      // Check that authorization clause does not reference undefined identities
      for (authDecl <- transition.authorized) {
        val ids = authDecl.extractIdentities
        val unknownIds = ids.diff(principals)
        if (unknownIds.nonEmpty) {
          return Some(s"Transition ${transition.name} references unknown identities: ${unknownIds.mkString(", ")}")
        }
      }

      // Check that transition body doesn't reference undefined fields
      // And type check all field assignments
      for (assignments <- transition.body) {
        for (assignment <- assignments) {
          assignment.leftSide.getType(localContext) match {
            case Left(err) => return Some(s"Type error in body of transition ${transition.name}: $err")
            case Right(leftTy) => assignment.rightSide.getType(localContext) match {
              case Left(err) => return Some(s"Type error in body of transition ${transition.name}: $err")
              case Right(rightTy) => if (leftTy != rightTy) {
                return Some(s"Type error in body of transition ${transition.name}: " +
                  s"Left-hand type $leftTy does not match right-hand type $rightTy")
              }
            }
          }
        }
      }
    }
    None
  }

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
