package edu.berkeley.cs.rise.bcdsl

import scala.util.parsing.combinator.JavaTokenParsers

object StateMachineParser extends JavaTokenParsers {
  def stripQuotes(s: String): String = if (s.startsWith("\"") && s.endsWith("\"")) {
    s.substring(1, s.length - 1)
  } else {
    s
  }

  def dataTypeDecl: Parser[DataType] = "Identity" ^^^ Identity |
    "Int" ^^^ Int |
    "String" ^^^ String |
    "Timestamp" ^^^ Timestamp |
    "Timespan" ^^^ Timespan |
    "Bool" ^^^ Bool |
    "Mapping" ~ "[" ~> dataTypeDecl ~ "," ~ dataTypeDecl <~ "]" ^^ { case keyType ~ "," ~ valueType => Mapping(keyType, valueType) }

  def variableDecl: Parser[Variable] = ident ~ ":" ~ dataTypeDecl ^^ { case name ~ ":" ~ ty => Variable(name, ty) }

  def mappingRef: Parser[MappingRef] = ident ~ "[" ~ logicalExpression <~ "]" ^^ { case name ~ "[" ~ key  => MappingRef(name, key)}

  def fieldList: Parser[Seq[Variable]] = "data" ~ "{" ~> rep(variableDecl) <~ "}"

  def valueDecl: Parser[SimpleValue] = "now" ^^^ Now |
    "sender" ^^^ Sender |
    "true" ^^^ BoolConst(true) |
    "false" ^^^ BoolConst(false) |
    "seconds" ^^^ Second |
    "minutes" ^^^ Minute |
    "hours" ^^^ Hour |
    "days" ^^^ Day |
    wholeNumber ^^ { s => IntConst(s.toInt) } |
    stringLiteral ^^ { s => StringLiteral(stripQuotes(s)) } |
    mappingRef |
    ident ^^ FieldRef

  def assignable: Parser[Assignable] = mappingRef | ident ^^ FieldRef

  def multDiv: Parser[ArithmeticOperator] = "*" ^^^ Multiply | "/" ^^^ Divide

  def plusMinus: Parser[ArithmeticOperator] = "+" ^^^ Plus | "-" ^^^ Minus

  def booleanOp: Parser[LogicalOperator] = "&&" ^^^ And | "||" ^^^ Or

  def comparator: Parser[LogicalOperator] = "==" ^^^ Equal |
    "!=" ^^^ NotEqual |
    "<" ^^^ LessThan |
    "<=" ^^^ LessThanOrEqual |
    ">" ^^^ GreaterThan |
    ">=" ^^^ GreaterThanOrEqual

  def factor: Parser[Expression] = valueDecl ^^ ValueExpression | "(" ~> arithmeticExpression <~ ")"

  def term: Parser[Expression] = chainl1(factor, multDiv ^^
    (op => (left: Expression, right: Expression) => ArithmeticExpression(left, op, right)))

  def arithmeticExpression: Parser[Expression] = chainl1(term, plusMinus ^^
    (op => (left: Expression, right: Expression) => ArithmeticExpression(left, op, right)))

  def atom: Parser[Expression] = arithmeticExpression | "(" ~> logicalExpression <~ ")"

  def clause: Parser[Expression] = chainl1(atom, comparator ^^
    (op => (left: Expression, right: Expression) => LogicalExpression(left, op, right)))

  def logicalExpression: Parser[Expression] = chainl1(clause, booleanOp ^^
    (op => (left: Expression, right: Expression) => LogicalExpression(left, op, right)))

  def authClause: Parser[AuthDecl] = ident ^^ AuthValue | "(" ~> authExpression <~ ")"

  def authExpression: Parser[AuthDecl] = chainl1(authClause, booleanOp ^^
    (op => (left: AuthDecl, right: AuthDecl) => AuthCombination(left, op, right)))

  def parameterList: Parser[Seq[Variable]] = "(" ~> rep1sep(variableDecl, ",") <~ ")"

  def stateChange: Parser[(Option[String], Option[Seq[Variable]], String)] =
    opt(ident) ~ "->" ~ opt(parameterList) ~ ident ^^ { case origin ~ "->" ~ parameters ~ destination =>
      (origin, parameters, destination)
    }

  def authAnnotation: Parser[AuthDecl] = "authorized" ~ "[" ~> authExpression <~ "]"

  def guardAnnotation: Parser[Expression] = "requires" ~ "[" ~> logicalExpression <~ "]"

  def assignment: Parser[Assignment] = assignable ~ "=" ~ (arithmeticExpression | logicalExpression) ^^ { case lhs ~ "=" ~ rhs => Assignment(lhs, rhs)}

  def transitionBody: Parser[Seq[Assignment]] = "{" ~> rep(assignment) <~ "}"

  def transition: Parser[Transition] = opt("auto") ~ stateChange ~ opt(authAnnotation) ~
    opt(guardAnnotation) ~ opt(transitionBody) ^^ { case autoStmt ~ ((origin, parameters, destination)) ~ auth ~ guard ~ body =>
    Transition(origin, destination, parameters, auth, autoStmt.isDefined, guard, body)
  }

  def stateMachine: Parser[StateMachine] = fieldList ~ rep(transition) ^^ { case fields ~ transitions => StateMachine(fields, transitions) }
}
