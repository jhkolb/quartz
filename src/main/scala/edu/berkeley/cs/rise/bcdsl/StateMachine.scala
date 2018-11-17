package edu.berkeley.cs.rise.bcdsl

case class Variable(name: String, ty: DataType)

case class Assignment(name: String, value: Expression)

case class Transition(origin: Option[String], destination: String, parameters: Option[Seq[Variable]],
                      authorized: Option[AuthDecl], guard: Option[Expression], body: Option[Seq[Assignment]]) {
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
    val initialState = initialTransitions.head.destination
    val allStates = transitions.foldLeft(Set.empty[String]) { (states, transition) =>
      transition.origin match {
        case Some(o) => states + (o, transition.destination)
        case None => states + transition.destination
      }
    }
    // Check that all states are reachable from initial state
    val reachableStates = depthFirstSearch(initialState, transitions)
    val unreachableStates = allStates.diff(reachableStates)
    if (unreachableStates.nonEmpty) {
      return Some("Unreachable states: " + unreachableStates.mkString(", "))
    }
    None

    val principals = fields.filter(_.ty == Identity).map(_.name).toSet
    val context = fields.foldLeft(Map.empty[String, DataType]) { (ctx, field) =>
      ctx + (field.name -> field.ty)
    }

    for (transition <- transitions) {
      val localContext = transition.parameters match {
        case None => context
        case Some(params) => params.foldLeft(context) { (ctxt, param) =>
          ctxt + (param.name -> param.ty)
        }
      }

      // Check that transition guard is correctly typed
      transition.guard match {
        case Some(expr) => expr.getType(localContext) match {
          case Left(err) => return Some(s"Invalid guard in transition ${transition.name}: $err")
          case Right(Bool) => ()
          case Right(ty) => return Some(s"Guard in transition ${transition.name} is of non-boolean type $ty")
        }
        case None => ()
      }

      // Check that authorization clause does not reference undefined identities
      transition.authorized match {
        case Some(authDecl) =>
          val ids = authDecl.extractIdentities
          val unknownIds = ids.diff(principals)
          if (unknownIds.nonEmpty) {
            return Some(s"Transition ${transition.name} references unknown identities: ${unknownIds.mkString(", ")}")
          }
        case None => ()
      }

      // Check that transition body doesn't reference undefined fields
      // And type check all field assignments
      transition.body match {
        case Some(assignments) => for (assignment <- assignments) {
          val fieldType = context.get(assignment.name)
          val valueType = assignment.value.getType(localContext)
          fieldType match {
            case None => return Some(s"Transition ${transition.name} references unknown field ${assignment.name}")
            case Some(fieldTy) => valueType match {
              case Left(errMsg) => return Some(s"Transition ${transition.name}: " +
                s"Type error in assignment to field ${assignment.name}: $errMsg")
              case Right(valueTy) => if (fieldTy != valueTy) {
                return Some(s"Transition ${transition.name} attempts to assign value of type $valueTy" +
                  s" to field '${assignment.name}' of type $fieldTy")
              }
            }
          }
        }

        case None => ()
      }
    }
    None
  }

  private def depthFirstSearch(startState: String, transitions: Seq[Transition]): Set[String] = {
    val adjacencyList = transitionsToAdjList(transitions)
    visit(startState, adjacencyList, Set.empty[String])
  }

  private def transitionsToAdjList(transitions: Seq[Transition]): Map[String, Set[String]] =
    transitions.foldLeft(Map.empty[String, Set[String]]) { (m, transition) =>
      transition.origin match {
        case None => m
        case Some(o) => m + (o -> (m.getOrElse(o, Set.empty[String]) + transition.destination))
      }
    }

  private def visit(state: String, adjacencyList: Map[String, Set[String]], visited: Set[String]): Set[String] =
    adjacencyList.get(state) match {
      case None => visited + state
      case Some(neighbors) =>
        val newVisited = visited + state
        neighbors.diff(newVisited).foldLeft(newVisited) { (currentVisited, neighbor) =>
          currentVisited ++ visit(neighbor, adjacencyList, currentVisited)
        }
    }
}
