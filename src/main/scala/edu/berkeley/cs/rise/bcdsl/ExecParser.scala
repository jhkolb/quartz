package edu.berkeley.cs.rise.bcdsl

object ExecParser extends App {
  override def main(args: Array[String]): Unit = {
    val input =
      """
         data {
           foo: Int
           bar: String
           baz: Identity
         }

         configured -> running
         authorized [ zz ]
         requires [ x < 4 && (z < 70 || y > 91) ] {}

         running -> stopped
         authorized [ admin ]
         requires [ true == false ] {
            powerLevel = "Over 9000"
            answer = 42
         }
      """.stripMargin

    StateMachineParser.parseAll(StateMachineParser.stateMachine, input) match {
      case StateMachineParser.Failure(msg, remaining) =>
        println(s"Syntax error: $msg at input ${remaining.first.toString}")
        System.exit(1)

      case StateMachineParser.Error(msg, _) =>
        println("Parser error: " + msg)
        System.exit(1)

      case StateMachineParser.Success(fieldList, _) =>
        println("Result:")
        println(fieldList)
    }
  }
}
