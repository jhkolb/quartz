package edu.berkeley.cs.rise.bcdsl

case class Field(name: String, ty: DataType)

case class Assignment(name: String, value: Expression)

case class Transition(origin: Option[String], destination: String, authorized: Option[AuthDecl],
                      guard: Option[Expression], body: Option[Seq[Assignment]])

case class StateMachine(fields: Seq[Field], transitions: Seq[Transition])
