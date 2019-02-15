package edu.berkeley.cs.rise.bcdsl

import org.scalatest.FunSuite

import scala.io.Source

class ParserSpec extends FunSuite {
  def parseFile(name: String): Specification = {
    val input = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(name)).mkString
    SpecificationParser.parseAll(SpecificationParser.specification, input) match {
      case SpecificationParser.Failure(msg, remaining) =>
        val nextInput = remaining.source.toString.substring(remaining.offset).takeWhile(_ != '\n')
        fail(s"Syntax Error: $msg at ${remaining.pos.toString} ($nextInput)")
      case SpecificationParser.Error(msg, _) =>
        fail(s"Parser Error: $msg")
      case SpecificationParser.Success(specification, _) =>
        specification
    }
  }

  test("Parsing a minimal state machine") {
    val spec = parseFile("minimal.txt")
    println(spec)
    println("------------")
    spec.stateMachine.validate() match {
      case None =>
        println(Solidity.writeSpecification(spec))
        println("----")
        println(PlusCal.writeSpecification(spec))
      case Some(err) => println(err)
    }
  }

  test("Parsing equipment warranty state machine") {
    val spec = parseFile("equipment.txt")
    println(spec)
    println("------------")
    spec.stateMachine.validate() match {
      case None =>
        println(Solidity.writeSpecification(spec))
        println("----")
        println(PlusCal.writeSpecification(spec))
      case Some(err) => println(err)
    }
  }

  test("Parsing auction state machine") {
    val spec = parseFile("auction.txt")
    println(spec)
    println("---------")
    spec.stateMachine.validate() match {
      case None =>
        println(Solidity.writeSpecification(spec))
        println("----")
        println(PlusCal.writeSpecification(spec))
        println("----")
        println(TLA.writeSpecificationToAux(spec))
        println("----")
        println(TLA.writeSpecificationToConfig(spec))
      case Some(err) => println(err)
    }
  }
}
