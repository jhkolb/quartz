package edu.berkeley.cs.rise.bcdsl

case class Specification(name: String, stateMachine: StateMachine, invariants: Option[Seq[LTLProperty]]) {}
