package edu.berkeley.cs.rise.bcdsl

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val configuration = ArgumentParser.parseArguments(args)
    if (configuration.isEmpty) {
      // Command parser already prints out error and usage
      System.exit(-1)
    }

    val specificationInput = Source.fromFile(configuration.get.inputFile)
    val parseResult = SpecificationParser.parseAll(SpecificationParser.specification, specificationInput.mkString)
    specificationInput.close()
    parseResult match {
      case SpecificationParser.Failure(msg, remaining) =>
        val nextInput = remaining.source.toString.substring(remaining.offset).takeWhile(_ != '\n')
        println(s"Syntax Error: $msg at ${remaining.pos.toString} ($nextInput)")
        System.exit(-1)

      case SpecificationParser.Error(msg, _) =>
        println(s"Parser error: $msg")
        System.exit(-1)

      case SpecificationParser.Success(specification, _) =>
        specification.validate().foreach { errMsg =>
          println(errMsg)
          System.exit(-1)
        }

        if (configuration.get.toSolidity) {
          Utils.writeStringToFile(s"${specification.name}.sol", Solidity.writeSpecification(specification))
        }

        if (configuration.get.toTLA) {
          val generatedTla = TLA.translatePlusCal(specification.name, PlusCal.writeSpecification(specification))
          val doctoredTla = TLA.modifyGeneratedTLA(generatedTla)

          Utils.writeStringToFile(s"${specification.name}.tla", doctoredTla)
          Utils.writeStringToFile(s"${specification.name}MC.tla", TLA.writeSpecificationToAux(specification))
          Utils.writeStringToFile(s"${specification.name}MC.cfg", TLA.writeSpecificationToConfig(specification))
        }
    }
  }
}
