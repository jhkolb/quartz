package edu.berkeley.cs.rise.bcdsl

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets

import org.scalatest.FunSuite

import scala.io.Source

object ParserSpec {
  private val TEST_FILES: Seq[String] = Seq(
    "minimal.txt",
    "equipment.txt",
    "auction.txt",
    "simpleMultiSig.txt",
    "majorityMultiSig.txt",
    "strictMultiSig.txt",
  )

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

  test("Parsing all test files") {
    new File(ParserSpec.OUTPUT_DIR).mkdir()
    ParserSpec.TEST_FILES.foreach { fileName =>
      val spec = parseFile(fileName)
      writeFile(spec.name + ".spec", spec.toString)

      spec.stateMachine.validate() match {
        case None =>
          val plusCal = PlusCal.writeSpecification(spec)
          val generatedTla = TLA.translatePlusCal(spec.name, plusCal)

          writeFile(s"${spec.name}.tla", TLA.modifyGeneratedTLA(generatedTla))
          writeFile(s"${spec.name}MC.cfg", TLA.writeSpecificationToAux(spec))
          writeFile(s"${spec.name}MC.tla", TLA.writeSpecificationToAux(spec))

          writeFile(spec.name + ".sol", Solidity.writeSpecification(spec))

        case Some(err) =>
          println(s"Error in $fileName: $err")
      }
    }
  }

  private def writeFile(fileName: String, contents: String): Unit = {
    val writer = new PrintWriter(s"${ParserSpec.OUTPUT_DIR}/$fileName", StandardCharsets.UTF_8)
    writer.println(contents)
    writer.close()
  }
}
