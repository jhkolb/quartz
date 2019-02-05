package edu.berkeley.cs.rise.bcdsl

case class Variable(name: String, ty: DataType)

case class Transition(name: String, origin: Option[String], destination: String, parameters: Option[Seq[Variable]],
                      authorized: Option[AuthDecl], auto: Boolean, guard: Option[Expression],
                      body: Option[Seq[Statement]]) {
  val description: String = s"'${origin.getOrElse("")}' -> '$destination'"
}

case class StateMachine(fields: Seq[Variable], transitions: Seq[Transition]) {
  val states: Set[String] = transitions.foldLeft(Set.empty[String]) { (currentStates, transition) =>
    currentStates ++ transition.origin.toSet + transition.destination
  }

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
    val adjacencyList = StateMachine.transitionsToAdjList(transitions)
    val simpleAdjacencyList = adjacencyList.mapValues(_.map(_.destination))
    val reachableStates = StateMachine.dfsVisit(initialState, simpleAdjacencyList, Set.empty[String])
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
        return Some(s"Automatic transition ${transition.description} cannot have authorization restriction")
      }
      // Check that transition is not designated as automatic but lacks a guard
      if (transition.auto && transition.guard.isEmpty) {
        return Some(s"Automatic transition ${transition.description} lacks a guard")
      }

      // Check that transition guard is correctly typed
      for (expr <- transition.guard) {
        expr.getType(localContext) match {
          case Left(err) => return Some(s"Invalid guard in transition ${transition.description}: $err")
          case Right(Bool) => ()
          case Right(ty) => return Some(s"Guard in transition ${transition.description} is of non-boolean type $ty")
        }
      }

      // Check that authorization clause does not reference undefined identities
      for (authDecl <- transition.authorized) {
        val ids = authDecl.extractIdentities
        val unknownIds = ids.diff(principals)
        if (unknownIds.nonEmpty) {
          return Some(s"Transition ${transition.description} references unknown identities: ${unknownIds.mkString(", ")}")
        }
      }

      // Check that transition body doesn't reference undefined fields
      // And type check all field assignments
      for ((statement, i) <- transition.body.getOrElse(Seq.empty[Statement]).zipWithIndex) {
        statement match {
          case Assignment(lhs, rhs) =>
            val leftRes = lhs.getType(localContext)
            val rightRes = rhs.getType(localContext)
            (leftRes, rightRes) match {
              case (Left(err), _) => return Some(StateMachine.makeTypeErrMsg(transition, i, err))
              case (_, Left(err)) => return Some(StateMachine.makeTypeErrMsg(transition, i, err))
              case (Right(leftTy), Right(rightTy)) if leftTy != rightTy => return Some(StateMachine.makeTypeErrMsg(transition, i,
                s"Left-hand type $leftTy does not match right=hand type of $rightTy"))
              case (Right(_), Right(_)) => ()
            }

          case Send(destination, amount) =>
            val destRes = destination.getType(localContext)
            val amountRes = amount.getType(localContext)
            (destRes, amountRes) match {
              case (Left(err), _) => return Some(StateMachine.makeTypeErrMsg(transition, i, err))
              case (_, Left(err)) => return Some(StateMachine.makeTypeErrMsg(transition, i, err))
              case (Right(Identity), Right(Int)) => () // This is the case we want
              case (Right(destTy), Right(Int)) if destTy != Identity => return Some(StateMachine.makeTypeErrMsg(transition, i,
                s"Expected send destination of type Identity, but found $destTy"))
              case (Right(Identity), Right(amountTy)) => return Some(StateMachine.makeTypeErrMsg(transition, i,
                s"Expected amount destination of type Int, but found $amountTy"))
            }
        }
      }
    }
    None
  }
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

  private def makeTypeErrMsg(transition: Transition, idx: Int, msg: String): String =
    s"Type error on statement ${idx + 1} of transition ${transition.description}: $msg"
}
