package edu.berkeley.cs.rise.bcdsl

import org.scalatest.FunSuite

import scala.io.Source

class ParserSpec extends FunSuite {
  def parseFile(name: String): StateMachine = {
    val input = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name)).mkString
    StateMachineParser.parseAll(StateMachineParser.stateMachine, input) match {
      case StateMachineParser.Failure(msg, remaining) =>
        fail(s"Syntax Error: $msg at input ${remaining.first.toString}")
      case StateMachineParser.Error(msg, _) =>
        fail(s"Parser Error: $msg")
      case StateMachineParser.Success(stateMachine, _) =>
        stateMachine
    }
  }

  test("Parsing a minimal state machine") {
    println(parseFile("minimal.txt"))
  }

  test("Parsing equipment warranty state machine") {
    println(parseFile("equipment.txt"))
  }
}
