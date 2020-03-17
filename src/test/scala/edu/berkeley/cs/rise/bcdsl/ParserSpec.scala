package edu.berkeley.cs.rise.bcdsl

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.io.Source

object ParserSpec {
  private val OUTPUT_DIR = "testOutput"
}

class ParserSpec extends FunSuite with BeforeAndAfterAll {
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

  override def beforeAll(): Unit = {
    new File(ParserSpec.OUTPUT_DIR).mkdir()
  }

  test("Mimimal contract with type error") {
    val spec = parseFile("minimal.qtz")
    testSpecification(spec, errorExpected = true)
  }

  test("Simple equipment management contract") {
    val spec = parseFile("equipment.qtz")
    testSpecification(spec)
  }

  test("Auction version 1") {
    val spec = parseFile("auction.qtz")
    testSpecification(spec)
  }

  test("Auction version 2") {
    val spec = parseFile("auction2.qtz")
    testSpecification(spec)
  }

  test("Auction version 3") {
    val spec = parseFile("auction3.qtz")
    testSpecification(spec)
  }

  test("Simple multi-signature wallet") {
    val spec = parseFile("simpleMultiSig.qtz")
    testSpecification(spec)
  }

  test("Strict multi-signature wallet") {
    val spec = parseFile("strictMultiSig.qtz")
    testSpecification(spec)
  }

  test("Contract with simple structs") {
    val spec = parseFile("struct.qtz")
    testSpecification(spec, errorExpected = true)
  }

  test("Simple timestamp arithmetic") {
    val spec = parseFile("timestampMath.qtz")
    testSpecification(spec)
  }

  test("Field of undefined struct type") {
    val spec = parseFile("undefinedStructField.qtz")
    testSpecification(spec, errorExpected = true)
  }

  test("Transition parameter of undefined struct type") {
    val spec = parseFile("undefinedStructParam.qtz")
    testSpecification(spec, errorExpected = true)
  }
}
