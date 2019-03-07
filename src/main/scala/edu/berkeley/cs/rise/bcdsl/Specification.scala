package edu.berkeley.cs.rise.bcdsl

case class Specification(name: String, stateMachine: StateMachine, invariants: Option[Seq[LTLProperty]]) {}

object Specification {
  // Special keywords that cannot appear as ordinary variables
  val RESERVED_VALUES: Map[String, DataType] = Map[String, DataType](
    "balance" -> Int,
    "now" -> Timestamp,
    "sender" -> Identity,
  )

  // Special transition parameters that must have a specific type if used
  val CONSTRAINED_PARAMS: Map[String, DataType] = Map[String, DataType](
    "tokens" -> Int,
  )
}
