package edu.berkeley.cs.rise.bcdsl

import scala.util.parsing.combinator.JavaTokenParsers

object StateMachineParser extends JavaTokenParsers {
  def dataTypeDecl: Parser[DataType] = "Identity" ^^ { _ => Identity } |
      "Int" ^^^ Int |
      "String" ^^^ String
      "Timestamp" ^^^ Timestamp
      "Timespan" ^^^ Timespan
      "Bool" ^^^ Bool

  def fieldDecl: Parser[Field] = ident ~ ":" ~ dataTypeDecl ^^ { case name ~ ":" ~ ty => Field(name, ty) }

  def fieldList: Parser[Seq[Field]] = "data" ~ "{" ~> rep(fieldDecl) <~ "}"

  def valueDecl: Parser[SimpleValue] = "now" ^^^ Now |
      "true" ^^^ BoolConst(true) |
      "false" ^^^ BoolConst(false) |
      wholeNumber ^^  { s => IntConst(s.toInt) } |
      stringLiteral ^^ StringLiteral |
      ident ^^ FieldRef

  def arithmeticOp1: Parser[ArithmeticOperator] = "*" ^^^ Multiply | "/" ^^^ Divide

  def arithmeticOp2: Parser[ArithmeticOperator] = "+" ^^^ Plus | "-" ^^^ Minus

  def logicalOp1: Parser[LogicalOperator] = "&&" ^^^ And | "||" ^^^ Or

  def logicalOp2: Parser[LogicalOperator] = "==" ^^^ Equal |
    "!=" ^^^ NotEqual |
    "<" ^^^ LessThan |
    "<=" ^^^ LessThanOrEqual |
    ">" ^^^ GreaterThan |
    ">=" ^^^  GreaterThanOrEqual
  def timespanConst: Parser[Timespan] = "Second" ^^^ Second |
      "Minute" ^^^ Minute |
      "Hour" ^^^ Hour |
      "Day" ^^^ Day |
      "Week" ^^^ Week

  def factor: Parser[Expression] = valueDecl ^^ ValueExpression | "(" ~> arithmeticExpression <~ ")"

  def term: Parser[Expression] = chainl1(factor, arithmeticOp1 ^^
      (op => (left: Expression, right: Expression) => ArithmeticExpression(left, op, right)))

  def arithmeticExpression: Parser[Expression] = chainl1(term, arithmeticOp2 ^^
      (op => (left: Expression, right: Expression) => ArithmeticExpression(left, op, right)))

  def atom: Parser[Expression] = valueDecl ^^ ValueExpression | "(" ~> logicalExpression <~ ")"

  def clause: Parser[Expression] = chainl1(atom, logicalOp1 ^^
      (op => (left: Expression, right: Expression) => LogicalExpression(left, op, right)))

  def logicalExpression: Parser[Expression] = chainl1(clause, logicalOp2 ^^
      (op => (left: Expression, right: Expression) => LogicalExpression(left, op, right)))

  def authClause: Parser[AuthDecl] = ident ^^ AuthValue | "(" ~> authDecl <~ ")"

  def authExpression: Parser[AuthDecl] = chainl1(authClause, logicalOp2 ^^
      (op => (left: AuthDecl, right: AuthDecl) => AuthCombination(left, op, right)))

  def stateChange: Parser[(Option[String], String)] = opt(ident) ~ "->" ~ ident ^^
      { case origin ~ "->" ~ destination => (origin, destination) }

  def authDecl: Parser[AuthDecl] = "authorized" ~ "[" ~> authExpression <~ "]"

  def guardDecl: Parser[Expression] = "requires" ~ "[" ~> logicalExpression <~ "]"

  def assignment: Parser[Assignment] = ident ~ "=" ~ (arithmeticExpression | logicalExpression) ^^
      { case name ~ "=" ~ expr => Assignment(name, expr) }

  def transitionBody: Parser[Seq[Assignment]] = "{" ~> rep(assignment) <~ "}"

  def transition: Parser[Transition] = stateChange ~ opt(authDecl) ~ opt(guardDecl) ~ transitionBody ^^
      { case (origin, destination) ~ auth ~ guard ~ body => Transition(origin, destination, auth, guard, body) }

  def stateMachine: Parser[StateMachine] = fieldList ~ rep(transition) ^^
      { case fields ~ transitions => StateMachine(fields, transitions) }
}
