package edu.berkeley.cs.rise.bcdsl

import java.io._

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val configuration = ArgumentParser.parseArguments(args)
    if (configuration.isEmpty) {
      // Command parser already prints out error and usage
      System.exit(-1)
    }

    val specificationInput = Source.fromFile(configuration.get.inputFile).mkString
    val parseResult = SpecificationParser.parseAll(SpecificationParser.specification, specificationInput)
    parseResult match {
      case SpecificationParser.Failure(msg, remaining) =>
        val nextInput = remaining.source.toString.substring(remaining.offset).takeWhile(_ != '\n')
        println(s"Syntax Error: $msg at ${remaining.pos.toString} ($nextInput)")
        System.exit(-1)

      case SpecificationParser.Error(msg, _) =>
        println(s"Parser error: $msg")
        System.exit(-1)

      case SpecificationParser.Success(specification, _) =>
        specification.stateMachine.validate().foreach { errMsg =>
          println(errMsg)
          System.exit(-1)
        }

        if (configuration.get.toSolidity) {
          writeStringToFile(s"${specification.name}.sol", Solidity.writeSpecification(specification))
        }

        if (configuration.get.toTLA) {
          writeStringToFile(s"${specification.name}.tla", PlusCal.writeSpecification(specification))
          writeStringToFile("MC.tla", TLA.writeSpecificationToAux(specification))
          writeStringToFile("MC.cfg", TLA.writeSpecificationToConfig(specification))
        }
    }
  }

  private def writeStringToFile(fileName: String, body: String): Unit = {
    val writer = new PrintWriter(fileName, "UTF-8")
    try {
      writer.print(body)
    } catch {
      case e: IOException => println(s"Failed to write Solidity file: ${e.getMessage}")
    } finally {
      writer.close()
    }
  }
}
