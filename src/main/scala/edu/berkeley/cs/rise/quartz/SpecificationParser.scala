package edu.berkeley.cs.rise.quartz

import scala.util.parsing.combinator.JavaTokenParsers

object SpecificationParser extends JavaTokenParsers {
  private def stripQuotes(s: String): String = if (s.startsWith("\"") && s.endsWith("\"")) {
    s.substring(1, s.length - 1)
  } else {
    s
  }

  def dataTypeDecl: Parser[DataType] = "Identity" ^^^ Identity |
    "Int" ^^^ IntVar |
    "Uint" ^^^ UnsignedIntVar |
    "String" ^^^ String |
    "Timestamp" ^^^ Timestamp |
    "Timespan" ^^^ Timespan |
    "Bool" ^^^ Bool |
    "Mapping" ~ "[" ~> dataTypeDecl ~ "," ~ dataTypeDecl <~ "]" ^^ { case keyType ~ "," ~ valueType => Mapping(keyType, valueType) } |
    "Sequence" ~ "[" ~> dataTypeDecl <~ "]" ^^ (elementType => Sequence(elementType)) |
    "HashValue" ~ "[" ~> rep1sep(dataTypeDecl, ",") <~ "]" ^^ HashValue |
    ident ^^ Struct

  def variableDecl: Parser[Variable] = ident ~ ":" ~ dataTypeDecl ^^ { case name ~ ":" ~ ty => Variable(name, ty) }

  def fieldList: Parser[Seq[Variable]] = "data" ~ "{" ~> rep(variableDecl) <~ "}"

  def valueDecl: Parser[Expression] = "true" ^^^ BoolConst(true) |
    "false" ^^^ BoolConst(false) |
    "seconds" ^^^ Second |
    "minutes" ^^^ Minute |
    "hours" ^^^ Hour |
    "days" ^^^ Day |
    "[0-9]+".r ^^ { s => UnsignedIntConst(s.toInt) } |
    "-[0-9]+".r ^^ { s => IntConst(s.toInt) } |
    stringLiteral ^^ { s => StringLiteral(stripQuotes(s)) } |
    assignable

  def hashApplication: Parser[Hash] = "hash" ~ "(" ~> rep1sep(expression, ",") <~ ")" ^^ Hash

  def structDecl: Parser[(String, Map[String, DataType])] = "struct" ~> ident ~ "{" ~ rep(variableDecl) <~ "}" ^^ { case name ~ "{" ~ fields =>
    (name, fields.map(f => f.name -> f.ty).toMap)
  }

  private case class FieldAccess(fieldName: String)
  private case class KeyAccess(key: Expression)
  def structMapRef: Parser[Assignable] = ident ~ rep1("." ~> ident ^^ FieldAccess | "[" ~> expression <~ "]" ^^ KeyAccess) ^^ { case root ~ elems =>
    val rootReference = elems.head match {
      case FieldAccess(fieldName) => StructAccess(VarRef(root), fieldName)
      case KeyAccess(key: Expression) => MappingRef(VarRef(root), key)
    }

    elems.tail match {
      case Nil =>  rootReference
      case rest => rest.foldLeft(rootReference) { (prev, item) => item match {
        case FieldAccess(fieldName) => StructAccess(prev, fieldName)
        case KeyAccess(key) => MappingRef(prev, key)
      }}
    }
  }

  def assignable: Parser[Assignable] = structMapRef | ident ^^ VarRef

  def multDivMod: Parser[ArithmeticOperator] = "*" ^^^ Multiply | "/" ^^^ Divide | "%" ^^^ Modulo

  def plusMinus: Parser[ArithmeticOperator] = "+" ^^^ Plus | "-" ^^^ Minus

  def booleanOp: Parser[BooleanOperator] = "&&" ^^^ And | "||" ^^^ Or | "=>" ^^^ Implies

  def sequenceOp: Parser[SequenceOperator] = "in" ^^^ In | "not in" ^^^ NotIn

  def comparator: Parser[Comparator] = "==" ^^^ Equal |
    "!=" ^^^ NotEqual |
    ">=" ^^^ GreaterThanOrEqual |
    "<=" ^^^ LessThanOrEqual |
    "<" ^^^ LessThan |
    ">" ^^^ GreaterThan

  def sequenceSize: Parser[SequenceSize] = "size" ~ "(" ~> expression <~ ")" ^^ SequenceSize

  def ltlExpression: Parser[LTLExpression] = "max" ~ "(" ~> assignable <~ ")" ^^ LTLMax |
    "min" ~ "(" ~> assignable <~ ")" ^^ LTLMin |
    "sum" ~ "(" ~> assignable <~ ")" ^^ LTLSum

  def factor: Parser[Expression] = sequenceSize | hashApplication | ltlExpression | valueDecl | "(" ~> expression <~ ")"

  def term: Parser[Expression] = chainl1(factor, multDivMod ^^
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

  def authTerm: Parser[AuthExpression] = "any" ~> assignable ^^ AuthAny |
    "all" ~> assignable ^^ AuthAll |
    assignable ^^ IdentityRef |
    "(" ~> authExpression <~ ")"

  def authExpression: Parser[AuthExpression] = chainl1(authTerm, booleanOp ^^
    (op => (left: AuthExpression, right: AuthExpression) => AuthCombination(left, op, right)))

  def authAnnotation: Parser[AuthExpression] = "authorized" ~ "[" ~> authExpression <~ "]"

  def guardAnnotation: Parser[Expression] = "requires" ~ "[" ~> logicalExpression <~ "]"

  def assignment: Parser[Assignment] = assignable ~ "=" ~ expression ^^ { case lhs ~ "=" ~ rhs => Assignment(lhs, rhs) }

  def send: Parser[Send] = "send" ~ expression ~ "to" ~ expression ~ opt("consuming" ~> assignable) ^^
    { case "send" ~ amountExpr ~ "to" ~ destExpr ~ source => Send(destExpr, amountExpr, source) }

  def sendAndConsume: Parser[Send] = "sendAndConsume" ~ assignable ~ "to" ~ expression ^^
    { case "sendAndConsume" ~ amount ~ "to" ~ destExpr => Send(destExpr, amount, Some(amount)) }

  def sequenceAddition: Parser[SequenceAppend] = "add" ~ expression ~ "to" ~ assignable ^^
    { case "add" ~ element ~ "to" ~ set => SequenceAppend(set, element) }

  def sequenceClear: Parser[SequenceClear] = "clear" ~> assignable ^^ SequenceClear

  def ifArm: Parser[(Expression, Seq[Statement])] = "if" ~ "(" ~> logicalExpression ~ ")" ~ "{" ~ rep1(statement) <~ "}" ^^
    { case expr ~ ")" ~ "{" ~ stmts => (expr, stmts) }

  def elseArm: Parser[Seq[Statement]] = "else" ~ "{" ~> rep1(statement) <~ "}"

  def conditional: Parser[Conditional] = ifArm ~ opt(elseArm) ^^ { case (condition, ifBody) ~
    elseBody => Conditional(condition, ifBody, elseBody) }

  def statement: Parser[Statement] = conditional | assignment | send | sendAndConsume | sequenceAddition | sequenceClear

  def parameterList: Parser[Seq[Variable]] = "(" ~> rep1sep(variableDecl, ",") <~ ")"

  def stateChange: Parser[(Option[String], Option[Seq[Variable]], String)] =
    opt(ident) ~ "->" ~ opt(parameterList) ~ ident ^^ { case origin ~ "->" ~ parameters ~ destination =>
      (origin, parameters, destination)
    }

  def transitionBody: Parser[Seq[Statement]] = "{" ~> rep1(statement) <~ "}"

  def transition: Parser[Transition] = ident ~ ":" ~ stateChange ~ opt(authAnnotation) ~
    opt(guardAnnotation) ~ opt(transitionBody) ^^ { case name ~ ":" ~ ((origin, parameters, destination)) ~ auth ~ guard ~ body =>
    Transition(name, origin, destination, parameters, auth, guard, body)
  }

  def stateMachine: Parser[StateMachine] = rep(structDecl) ~ fieldList ~ rep(transition) ^^ { case structs ~ fields ~ transitions =>
    val structMap = structs.map { case (name, fields) => name -> fields }.toMap
    StateMachine(structMap, fields, transitions)
  }

  def ltlOperator: Parser[LTLOperator] = "[]" ^^^ Always |
    "<>" ^^^ Eventually

  def ltlProperty: Parser[LTLProperty] = ltlOperator ~ "(" ~ logicalExpression <~ ")" ^^ { case op ~ "(" ~ expr => LTLProperty(op, Right(expr)) } |
    ltlOperator ~ "(" ~ ltlProperty <~ ")" ^^ { case op ~ "(" ~ prop => LTLProperty(op, Left(prop)) }

  def propertySpec: Parser[Seq[LTLProperty]] = "properties" ~ "{" ~> rep(ltlProperty) <~ "}"

  def specification: Parser[Specification] = "contract" ~> ident ~ "{" ~ stateMachine ~ "}" ~ opt(propertySpec) ^^
    { case name ~ "{" ~ stateMachine ~ "}" ~ props => Specification(name, stateMachine, props) }
}
