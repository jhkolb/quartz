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
    "Mapping" ~ "[" ~> dataTypeDecl ~ "," ~ dataTypeDecl <~ "]" ^^ { case keyType ~ "," ~ valueType => Mapping(keyType, valueType) } |
    "Sequence" ~ "[" ~> dataTypeDecl <~ "]" ^^ (elementType => Sequence(elementType))

  def variableDecl: Parser[Variable] = ident ~ ":" ~ dataTypeDecl ^^ { case name ~ ":" ~ ty => Variable(name, ty) }

  // TODO this could probably be cleaner...
  // Assumes that any mappingRef starts with an identifier
  def mappingRef: Parser[MappingRef] = ident ~ rep1("[" ~> logicalExpression <~ "]") ^^ {
    case  root ~ (keyExpr::keyExprs) => keyExprs.foldLeft(MappingRef(VarRef(root), keyExpr)){ (ref, ke) => MappingRef(ref, ke) }
    case _ ~ Nil => throw new IllegalStateException("rep1 combinator in 'mappingRef' yielded empty list")
  }

  def fieldList: Parser[Seq[Variable]] = "data" ~ "{" ~> rep(variableDecl) <~ "}"

  def valueDecl: Parser[Expression] = "true" ^^^ BoolConst(true) |
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

  def booleanOp: Parser[BooleanOperator] = "&&" ^^^ And | "||" ^^^ Or

  def sequenceOp: Parser[SequenceOperator] = "in" ^^^ In | "not in" ^^^ NotIn

  def comparator: Parser[Comparator] = "==" ^^^ Equal |
    "!=" ^^^ NotEqual |
    ">=" ^^^ GreaterThanOrEqual |
    "<=" ^^^ LessThanOrEqual |
    "<" ^^^ LessThan |
    ">" ^^^ GreaterThan

  def sequenceSize: Parser[SequenceSize] = "size" ~ "(" ~> expression <~ ")" ^^ SequenceSize

  def factor: Parser[Expression] = sequenceSize | valueDecl | "(" ~> expression <~ ")"

  def term: Parser[Expression] = chainl1(factor, multDiv ^^
    (op => (left: Expression, right: Expression) => ArithmeticOperation(left, op, right)))

  // Note: It's possible we could do something fancy like declaring this as a Parser[ArithmeticExpression]
  // And enforcing more structure on the arithmetic/logical expressions at parse time
  // But let's leave this to the type checker, where it likely belongs
  def expression: Parser[Expression] = chainl1(term, plusMinus ^^
    (op => (left: Expression, right: Expression) => ArithmeticOperation(left, op, right)))

  def atom: Parser[Expression] = expression | "(" ~> logicalExpression <~ ")"

  def clause: Parser[Expression] = chainl1(atom, comparator ^^
    (op => (left: Expression, right: Expression) => LogicalOperation(left, op, right)))

  // l2 is short for "Level 2", as in lower operator precedence. Maybe need a better name
  def l2Clause: Parser[Expression] = chainl1(clause, sequenceOp ^^
    (op => (left: Expression, right: Expression) => LogicalOperation(left, op, right)))

  def logicalExpression: Parser[Expression] = chainl1(l2Clause, booleanOp ^^
    (op => (left: Expression, right: Expression) => LogicalOperation(left, op, right)))

  def authTerm: Parser[AuthExpression] = "any" ~> ident ^^ AuthAny |
    "all" ~> ident ^^ AuthAll |
    ident ^^ IdentityLiteral |
    "(" ~> authExpression <~ ")"

  def authExpression: Parser[AuthExpression] = chainl1(authTerm, booleanOp ^^
    (op => (left: AuthExpression, right: AuthExpression) => AuthCombination(left, op, right)))

  def authAnnotation: Parser[AuthExpression] = "authorized" ~ "[" ~> authExpression <~ "]"

  def guardAnnotation: Parser[Expression] = "requires" ~ "[" ~> logicalExpression <~ "]"

  def assignment: Parser[Assignment] = assignable ~ "=" ~ expression ^^ { case lhs ~ "=" ~ rhs => Assignment(lhs, rhs) }

  def send: Parser[Send] = "send" ~ expression ~ "to" ~ expression ~ opt("consuming" ~> assignable) ^^
    { case "send" ~ amountExpr ~ "to" ~ destExpr ~ source => Send(destExpr, amountExpr, source) }

  def sendAndConsume: Parser[Send] = "sendAndConsume" ~ assignable ~ "to" ~ expression ^^
    { case "sendAndConsume" ~ amount ~ "to" ~ destExpr => Send(destExpr, amount, Some(amount))}

  def sequenceAddition: Parser[SequenceAppend] = "add" ~ expression ~ "to" ~ expression ^^
    { case "add" ~ element ~ "to" ~ set => SequenceAppend(set, element) }

  def sequenceClear: Parser[SequenceClear] = "clear" ~> expression ^^ SequenceClear

  def statement: Parser[Statement] = assignment | send | sendAndConsume | sequenceAddition | sequenceClear

  def parameterList: Parser[Seq[Variable]] = "(" ~> rep1sep(variableDecl, ",") <~ ")"

  def stateChange: Parser[(Option[String], Option[Seq[Variable]], String)] =
    opt(ident) ~ "->" ~ opt(parameterList) ~ ident ^^ { case origin ~ "->" ~ parameters ~ destination =>
      (origin, parameters, destination)
    }

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
