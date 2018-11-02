package edu.berkeley.cs.rise.bcdsl

case class Field(name: String, ty: DataType)

case class Assignment(name: String, value: Expression)

case class Transition(origin: Option[String], destination: String, authorized: Option[AuthDecl],
                      guard: Option[Expression], body: Option[Seq[Assignment]])

case class StateMachine(fields: Seq[Field], transitions: Seq[Transition]) {
  lazy val states: Set[String] = transitions.foldLeft(Set.empty[String]) { (states, transition) =>
    transition.origin match {
      case None => states + transition.destination
      case Some(o) => states + (o, transition.destination)
    }
  }

  // Assumes there is exactly one transition without a specified origin
  val (initialTransition: Transition, standardTransitions: Seq[Transition]) =
    transitions.partition(_.origin.isEmpty) match {
      case (initials, standards) => (initials.head, standards)
    }

  def toSolidity: String = {
    val builder = new StringBuilder()
    builder.append("pragma solidity ^0.4.21;\n")
    builder.append("Contract AutoGen {\n")

    builder.append("enum State {\n")
    states foreach { state => builder.append(s"  $state,\n")}
    builder.append("}\n")

    fields foreach { f =>
      f.ty match {
        case Identity => builder.append("address")
        case Int => builder.append("int")
        case String => builder.append("bytes32")
        case Timestamp => builder.append("uint")
        case Bool => builder.append("bool")
        case Timespan => builder.append("uint")
      }
      builder.append(s" public ${f.name};\n\n")
    }
    builder.append("State public currentState;\n")

    builder.append("constructor() public {\n")
    builder.append(s"  currentState = ${initialTransition.destination};\n")
    initialTransition.body match {
      case None =>  ()
      case Some(assignments) => assignments foreach { assignment =>
        builder.append(s"  ${assignment.name} = ${Solidity.writeExpression(assignment.value)};\n")
      }
    }
    builder.append("}\n\n")

    standardTransitions foreach { transition =>
      builder.append(s"function ${transition.origin.get}_to_${transition.destination}() public {\n")
      builder.append(s"  currentState = ${transition.destination};\n")
      transition.body match {
        case None => ()
        case Some(assignments) => assignments foreach { assignment =>
          builder.append(s"  ${assignment.name} = ${Solidity.writeExpression(assignment.value)};\n")
        }
      }
      builder.append("}\n\n")
    }

    builder.toString
  }
}
