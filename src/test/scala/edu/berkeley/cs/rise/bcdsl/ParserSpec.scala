package edu.berkeley.cs.rise.bcdsl

import org.scalatest.FunSuite

import scala.io.Source

class ParserSpec extends FunSuite {
  def parseFile(name: String): StateMachine = {
    val input = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name)).mkString
    StateMachineParser.parseAll(StateMachineParser.stateMachine, input) match {
      case StateMachineParser.Failure(msg, remaining) =>
        val nextInput = remaining.source.toString.substring(remaining.offset).takeWhile(_ != '\n')
        fail(s"Syntax Error: $msg at ${remaining.pos.toString} ($nextInput)")
      case StateMachineParser.Error(msg, _) =>
        fail(s"Parser Error: $msg")
      case StateMachineParser.Success(stateMachine, _) =>
        stateMachine
    }
  }

  test("Parsing a minimal state machine") {
    val ast = parseFile("minimal.txt")
    println(ast)
    println("------------")
    println(ast.toSolidity)
  }

  test("Parsing equipment warranty state machine") {
    val ast = parseFile("equipment.txt")
    println(ast)
    println("------------")
    println(ast.toSolidity)
  }
}