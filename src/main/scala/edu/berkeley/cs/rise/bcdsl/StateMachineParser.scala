package edu.berkeley.cs.rise.bcdsl

import scala.util.parsing.combinator.JavaTokenParsers

object StateMachineParser extends JavaTokenParsers {
  def dataTypeDecl: Parser[DataType] = "Identity" ^^ { _ => Identity } |
      "Int" ^^ { _ => Int } |
      "String" ^^ { _ => String }
      "Timestamp" ^^ { _ => Timestamp }
      "Timespan" ^^ { _ => Timespan }
      "Bool" ^^ { _ => Bool }

  def fieldDecl: Parser[Field] = ident ~ ":" ~ dataTypeDecl ^^ { case name ~ ":" ~ ty => Field(name, ty) }

  def fieldList: Parser[Seq[Field]] = "data" ~ "{" ~> rep(fieldDecl) <~ "}"

  def valueDecl: Parser[SimpleValue] = "now" ^^ { _ => Now } |
      "true" ^^ { _ => BoolConst(true) } |
      "false" ^^ { _ => BoolConst(false) } |
      wholeNumber ^^ { s => IntConst(s.toInt) } |
      stringLiteral ^^ StringLiteral |
      ident ^^ FieldRef

  def logicalOp: Parser[LogicalOperator] = "==" ^^ { _ => Equal } |
      "!=" ^^ { _ => NotEqual } |
      "<" ^^ { _ => LessThan} |
      "<=" ^^ { _ => LessThanOrEqual } |
      ">" ^^ { _ => GreaterThan } |
      ">=" ^^ { _ => GreaterThanOrEqual } |
      "&&" ^^ { _ => And } |
      "||" ^^ { _ => Or }

  def arithmeticOp: Parser[ArithmeticOperator] = "+" ^^ { _ => Plus } |
      "-" ^^ { _ => Minus } |
      "*" ^^ { _ => Multiply } |
      "/" ^^ { _ => Divide }

  def timespanConst: Parser[Timespan] = "Second" ^^ { _ => Second } |
      "Minute" ^^ { _ => Minute } |
      "Hour" ^^ { _ => Hour } |
      "Day" ^^ { _ => Day } |
      "Week" ^^ { _ => Week }

  def expression: Parser[Expression] =
      expression ~ logicalOp ~ expression ^^ { case left ~ op ~ right => LogicalExpression(left, op, right) } |
      expression ~ arithmeticOp ~ expression ^^ { case left ~ op ~ right => ArithmeticExpression(left, op, right) } |
      valueDecl ^^ ValueExpression |
      "(" ~> expression <~ ")"

  def authExpression: Parser[AuthExpression] = ident ^^ AuthValue |
      authExpression ~ "and" ~ authExpression ^^ { case left ~ "and" ~ right => AuthConjunction(left, right) } |
      authExpression ~ "or" ~ authExpression ^^ { case left ~ "or" ~ right => AuthDisjunction(left, right) }
      "(" ~> authExpression <~ ")"

  def stateChange: Parser[(Option[String], String)] = opt(ident) ~ "->" ~ ident ^^
      { case origin ~ "->" ~ destination => (origin, destination) }

  def authDecl: Parser[AuthExpression] = "authorized" ~ "[" ~> authExpression <~ "]"

  def guardDecl: Parser[Expression] = "requires" ~ "[" ~> expression <~ "]"

  def assignment: Parser[Assignment] = ident ~ "=" ~ expression ^^ { case name ~ "=" ~ expr => Assignment(name, expr) }

  def transitionBody: Parser[Seq[Assignment]] = "{" ~> rep(assignment) <~ "}"

  def transition: Parser[Transition] = stateChange ~ opt(authDecl) ~ opt(guardDecl) ~ transitionBody ^^
      { case (origin, destination) ~ auth ~ guard ~ body => Transition(origin, destination, auth, guard, body) }

  def stateMachine: Parser[StateMachine] = fieldList ~ rep(transition) ^^
      { case fields ~ transitions => StateMachine(fields, transitions) }
}
