package edu.berkeley.cs.rise.bcdsl

case class Variable(name: String, ty: DataType)

case class Transition(name: String, origin: Option[String], destination: String, parameters: Option[Seq[Variable]],
                      authorized: Option[AuthExpression], auto: Boolean, guard: Option[Expression], body: Option[Seq[Statement]]) {
  val description: String = s"$name: '${origin.getOrElse("")}' -> '$destination'"

  private def makeTypeErrMsg(idx: Int, msg: String): String =
    s"Type error on statement ${idx + 1} of transition $description: $msg"

  def validate(context: Map[String, DataType]): Option[String] = {
    val paramsMap = parameters.fold(Map.empty[String, DataType]) { params =>
      params.map(variable => variable.name -> variable.ty).toMap
    }
    // Check that transition parameters do not shadow previously defined variables
    paramsMap.keySet.intersect(context.keySet).headOption.foreach { shadowed =>
      return Some(s"Transition $description shadows field or keyword $shadowed")
    }
    // And check that any constrained parameters are of the correct type
    val constrainedNames = paramsMap.keySet.intersect(Specification.CONSTRAINED_PARAMS.keySet)
    constrainedNames.foreach { name =>
      val actualTy = paramsMap(name)
      val expectedTy = Specification.CONSTRAINED_PARAMS(name)
      if (actualTy != expectedTy) {
        return Some(s"Special parameter $name is of type $actualTy when it must be of type $expectedTy")
      }
    }
    val localContext = context ++ paramsMap

    // Check that transition is not designated as automatic but has an authorization restriction
    if (auto && authorized.isDefined) {
      return Some(s"Automatic transition $description cannot have authorization restriction")
    }
    // Check that transition is not designated as automatic but lacks a guard
    if (auto && guard.isEmpty) {
      return Some(s"Automatic transition $description lacks a guard")
    }

    // Check that transition guard is correctly typed
    for (expr <- guard) {
      expr.getType(localContext) match {
        case Left(err) => return Some(s"Invalid guard in transition $description: $err")
        case Right(Bool) => ()
        case Right(ty) => return Some(s"Guard in transition $description is of non-boolean type $ty")
      }
    }

    // Check that authorization clause does not reference undefined identities or invalid types
    for (term <- authorized.fold(Set.empty[AuthTerm])(_.flatten)) {
      val name = term.getReferencedName
      context.get(name) match {
        case None => return Some(s"Transition $description references unknown identities $name")
        case Some(Identity) | Some(Sequence(Identity)) => ()
        case Some(ty) => return Some(s"Transition $description references invalid type $ty in authorization clause")
      }
    }

    // Type check all transition body statements
    for ((statement, i) <- body.getOrElse(Seq.empty[Statement]).zipWithIndex) {
      statement match {
        case Assignment(lhs, rhs) =>
          val leftRes = lhs.getType(localContext)
          val rightRes = rhs.getType(localContext)
          (leftRes, rightRes) match {
            case (Left(err), _) => return Some(makeTypeErrMsg(i, err))
            case (_, Left(err)) => return Some(makeTypeErrMsg(i, err))
            case (Right(leftTy), Right(rightTy)) if leftTy != rightTy => return Some(makeTypeErrMsg(i,
              s"Left-hand type $leftTy does not match right-hand type of $rightTy"))
            case (Right(_), Right(_)) => ()
          }

        case Send(sendDest, amount, source) =>
          sendDest.getType(localContext) match {
            case Left(err) => return Some(makeTypeErrMsg(i, err))
            case Right(destTy) if destTy != Identity =>
              return Some(makeTypeErrMsg(i, s"Expected send destination of type Identity but found $destTy"))
            case Right(_) => ()
          }

          amount.getType(localContext) match {
            case Left(err) => return Some(makeTypeErrMsg(i, err))
            case Right(amountTy) if amountTy != Int =>
              return Some(makeTypeErrMsg(i, s"Expected send amount of type Int, but found $amountTy"))
            case Right(_) => ()
          }

          source.foreach(_.getType(localContext) match {
            case Left(err) => return Some(makeTypeErrMsg(i, err))
            case Right(sourceTy) if sourceTy != Int =>
              return Some(makeTypeErrMsg(i, s"Expected send to consume var of type Int, but found type $sourceTy"))
            case Right(_) => ()
          })

        case SequenceAppend(sequence, element) => sequence.getType(localContext) match {
          case Left(err) => return Some(makeTypeErrMsg(i, err))
          case Right(Sequence(elementTy)) => element.getType(localContext) match {
            case Left(err) => return Some(makeTypeErrMsg(i, err))
            case Right(ty) if ty == elementTy => ()
            case Right(ty) =>
              return Some(makeTypeErrMsg(i, s"Cannot append $ty instance to sequence of $elementTy instances"))
          }
          case Right(ty) => return Some(makeTypeErrMsg(i, s"Cannot append to non-sequence type $ty"))
        }
      }
    }
    None
  }
}

case class StateMachine(fields: Seq[Variable], transitions: Seq[Transition]) {
  val states: Set[String] = transitions.foldLeft(Set.empty[String]) { (currentStates, transition) =>
    currentStates ++ transition.origin.toSet + transition.destination
  }

  def validate(): Option[String] = {
    // Check that no fields shadow a reserved value
    fields.map(_.name).toSet.intersect(Specification.RESERVED_VALUES.keySet).headOption.foreach { name =>
      return Some(s"State machine field $name shadows reserved value")
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

    val context = fields.foldLeft(Specification.RESERVED_VALUES) { (ctx, field) =>
      ctx + (field.name -> field.ty)
    }
    for (transition <- transitions) {
      transition.validate(context).foreach(err => return Some(err))
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
}
