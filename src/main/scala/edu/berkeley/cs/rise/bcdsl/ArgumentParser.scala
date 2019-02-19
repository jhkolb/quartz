package edu.berkeley.cs.rise.bcdsl

object ArgumentParser {
  case class Config(
    inputFile: String = "",
    toSolidity: Boolean = false,
    toTLA: Boolean = false,
  )

  private val parser = new scopt.OptionParser[Config]("BcDsl") {
    head("BcDsl", "0.1")

    opt[String]('i', "inputFile") required() action { (x, c) =>
      c.copy(inputFile = x)
    }

    opt[Unit]('s', "toSolidity") action { (_, c) =>
      c.copy(toSolidity = true)
    }

    opt[Unit]('t', "toTLA") action{ (_, c) =>
      c.copy(toTLA = true)
    }

    checkConfig { c =>
      if (!c.toSolidity && !c.toTLA) {
        failure("Must request at least one output target")
      } else {
        success
      }
    }
  }

  def parseArguments(args: Seq[String]): Option[Config] = parser.parse(args, Config())
}
