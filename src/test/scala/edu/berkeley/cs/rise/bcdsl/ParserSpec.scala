package edu.berkeley.cs.rise.bcdsl

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

import org.scalatest.FunSuite

import scala.io.Source

object ParserSpec {
  private val OUTPUT_DIR = "testOutput"
}

class ParserSpec extends FunSuite {
  def parseFile(name: String): Specification = {
    val input = Source.fromResource(name).mkString
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

  def testSpecification(spec: Specification, errorExpected: Boolean = false): Unit = {
    writeFile(spec.name + ".spec", spec.toString)

    spec.validate() match {
      case None =>
        val plusCal = PlusCal.writeSpecification(spec)
        val generatedTla = TLA.translatePlusCal(spec.name, plusCal)

        writeFile(s"${spec.name}.tla", TLA.modifyGeneratedTLA(generatedTla))
        writeFile(s"${spec.name}MC.tla", TLA.writeSpecificationToAux(spec))
        writeFile(s"${spec.name}MC.cfg", TLA.writeSpecificationToConfig(spec))

        writeFile(spec.name + ".sol", Solidity.writeSpecification(spec))

      case Some(err) =>
        if (!errorExpected) {
          fail(err)
        } else {
          println(s"Error in ${spec.name}: $err")
        }
    }
  }

  private def writeFile(fileName: String, contents: String): Unit = {
    val writer = new PrintWriter(s"${ParserSpec.OUTPUT_DIR}/$fileName", StandardCharsets.UTF_8)
    writer.println(contents)
    writer.close()
  }

  test("Mimimal contract with type error") {
    val spec = parseFile("minimal.txt")
    testSpecification(spec, errorExpected = true)
  }

  test("Simple equipment management contract") {
    val spec = parseFile("equipment.txt")
    testSpecification(spec)
  }

  test("Auction version 1") {
    val spec = parseFile("auction.txt")
    testSpecification(spec)
  }

  test("Auction version 2") {
    val spec = parseFile("auction2.txt")
    testSpecification(spec)
  }

  test("Auction version 3") {
    val spec = parseFile("auction3.txt")
    testSpecification(spec)
  }

  test("Simple multi-signature wallet") {
    val spec = parseFile("simpleMultiSig.txt")
    testSpecification(spec)
  }

  test("Strict multi-signature wallet") {
    val spec = parseFile("strictMultiSig.txt")
    testSpecification(spec)
  }

  test("Contract with simple structs") {
    val spec = parseFile("struct.txt")
    testSpecification(spec, errorExpected = true)
  }

  test("Simple timestamp arithmetic") {
    val spec = parseFile("timestampMath.txt")
    testSpecification(spec)
  }
}
