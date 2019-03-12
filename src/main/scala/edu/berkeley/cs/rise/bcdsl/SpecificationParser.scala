package edu.berkeley.cs.rise.bcdsl

import scala.util.parsing.combinator.JavaTokenParsers

object SpecificationParser extends JavaTokenParsers {
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

  def mappingRef: Parser[MappingRef] = ident ~ "[" ~ logicalExpression <~ "]" ^^ { case name ~ "[" ~ key => MappingRef(name, key) }

  def fieldList: Parser[Seq[Variable]] = "data" ~ "{" ~> rep(variableDecl) <~ "}"

  def valueDecl: Parser[SimpleValue] = "true" ^^^ BoolConst(true) |
    "false" ^^^ BoolConst(false) |
    "seconds" ^^^ Second |
    "minutes" ^^^ Minute |
    "hours" ^^^ Hour |
    "days" ^^^ Day |
    wholeNumber ^^ { s => IntConst(s.toInt) } |
    stringLiteral ^^ { s => StringLiteral(stripQuotes(s)) } |
    mappingRef |
    ident ^^ VarRef

  def assignable: Parser[Assignable] = mappingRef | ident ^^ VarRef

  def multDiv: Parser[ArithmeticOperator] = "*" ^^^ Multiply | "/" ^^^ Divide

  def plusMinus: Parser[ArithmeticOperator] = "+" ^^^ Plus | "-" ^^^ Minus

  def booleanOp: Parser[LogicalOperator] = "&&" ^^^ And | "||" ^^^ Or

  def comparator: Parser[LogicalOperator] = "==" ^^^ Equal |
    "!=" ^^^ NotEqual |
    ">=" ^^^ GreaterThanOrEqual |
    "<=" ^^^ LessThanOrEqual |
    "<" ^^^ LessThan |
    ">" ^^^ GreaterThan

  def factor: Parser[Expression] = valueDecl ^^ ValueExpression | "(" ~> arithmeticExpression <~ ")"

  def term: Parser[Expression] = chainl1(factor, multDiv ^^
    (op => (left: Expression, right: Expression) => ArithmeticExpression(left, op, right)))

  // Note: It's possible we could do something fancy like declaring this as a Parser[ArithmeticExpression]
  // And enforcing more structure on the arithmetic/logical expressions at parse time
  // But let's leave this to the type checker, where it likely belongs
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

  def assignment: Parser[Assignment] = assignable ~ "=" ~ (arithmeticExpression | logicalExpression) ^^ { case lhs ~ "=" ~ rhs => Assignment(lhs, rhs) }

  def send: Parser[Send] = "send" ~ arithmeticExpression ~ "to" ~ arithmeticExpression ~ opt("consuming" ~ assignable) ^^
    { case "send" ~ amountExpr ~ "to" ~ destExpr ~ source => source match {
        case None => Send(destExpr, amountExpr, None)
        case Some("consuming" ~ sourceRef) => Send(destExpr, amountExpr, Some(sourceRef))
        // This should never happen, just here to make match exhaustive
        case Some(_) => throw new IllegalStateException("Matched fallthrough pattern when parsing 'send'")
      }
    }

  def statement: Parser[Statement] = assignment | send

  def transitionBody: Parser[Seq[Statement]] = "{" ~> rep(statement) <~ "}"

  def transition: Parser[Transition] = opt("auto") ~ ident ~ ":" ~ stateChange ~ opt(authAnnotation) ~
    opt(guardAnnotation) ~ opt(transitionBody) ^^ { case autoStmt ~ name ~ ":" ~ ((origin, parameters, destination)) ~ auth ~ guard ~ body =>
    Transition(name, origin, destination, parameters, auth, autoStmt.isDefined, guard, body)
  }

  def stateMachine: Parser[StateMachine] = fieldList ~ rep(transition) ^^ { case fields ~ transitions => StateMachine(fields, transitions) }

  def ltlOperator: Parser[LTLOperator] = "[]" ^^^ Always |
    "<>" ^^^ Eventually

  def ltlProperty: Parser[LTLProperty] = ltlOperator ~ "(" ~ logicalExpression <~ ")" ^^ { case op ~ "(" ~ expr => LTLProperty(op, Right(expr)) } |
    ltlOperator ~ "(" ~ ltlProperty <~ ")" ^^ { case op ~ "(" ~ prop => LTLProperty(op, Left(prop)) }

  def propertySpec: Parser[Seq[LTLProperty]] = "properties" ~ "{" ~> rep(ltlProperty) <~ "}"

  def specification: Parser[Specification] = "contract" ~> ident ~ "{" ~ stateMachine ~ "}" ~ opt(propertySpec) ^^ { case name ~ "{" ~ stateMachine ~ "}" ~ props => Specification(name, stateMachine, props) }
}
