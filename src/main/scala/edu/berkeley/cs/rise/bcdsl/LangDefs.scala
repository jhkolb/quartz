package edu.berkeley.cs.rise.bcdsl

sealed trait DataType
case object Identity extends DataType
case object Int extends DataType
case object String extends DataType
case object Timestamp extends DataType
case object Bool extends DataType
case object Timespan extends DataType

sealed trait SimpleValue
case class FieldRef(name: String) extends SimpleValue
case class IntConst(value: Int) extends SimpleValue
case class StringLiteral(value: String) extends SimpleValue
case object Now  extends SimpleValue
case class BoolConst(value: Boolean) extends SimpleValue

sealed trait BinaryOperator

sealed trait LogicalOperator extends BinaryOperator
case object Equal extends LogicalOperator
case object NotEqual extends LogicalOperator
case object LessThan extends LogicalOperator
case object LessThanOrEqual extends LogicalOperator
case object GreaterThan extends LogicalOperator
case object GreaterThanOrEqual extends LogicalOperator
case object And extends LogicalOperator
case object Or extends LogicalOperator

sealed trait ArithmeticOperator extends BinaryOperator
case object Plus extends ArithmeticOperator
case object Minus extends ArithmeticOperator
case object Multiply extends ArithmeticOperator
case object Divide extends ArithmeticOperator

sealed trait Timespan
case object Second extends Timespan
case object Minute extends Timespan
case object Hour extends Timespan
case object Day extends Timespan
case object Week extends Timespan

sealed trait Expression
case class ValueExpression(value: SimpleValue) extends Expression
case class LogicalExpression(left: Expression, operator: LogicalOperator, right: Expression) extends Expression
case class ArithmeticExpression(left: Expression, operator: ArithmeticOperator, right: Expression) extends Expression

sealed trait AuthDecl
case class AuthValue(name: String) extends AuthDecl
case class AuthCombination(left: AuthDecl, operator: LogicalOperator, right: AuthDecl) extends AuthDecl
