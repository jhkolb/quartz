package edu.berkeley.cs.rise.bcdsl

sealed trait DataType
case object Identity extends DataType
case object Int extends DataType
case object String extends DataType
case object Timestamp extends DataType
case object Bool extends DataType
case object Timespan extends DataType
case class Mapping(keyType: DataType, valueType: DataType) extends DataType

trait Typed {
  def getType(context: Map[String, DataType]): Either[String, DataType]
}

sealed trait SimpleValue extends Typed
sealed trait Assignable extends SimpleValue

case class FieldRef(name: String) extends Assignable {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = context.get(name) match {
    case None => Left(s"Undefined field $name")
    case Some(ty) => Right(ty)
  }
}
case class IntConst(value: Int) extends SimpleValue {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Int)
}

case class StringLiteral(value: String) extends SimpleValue {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(String)
}

case object Now extends SimpleValue {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Timestamp)
}

case object Sender extends SimpleValue {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Identity)
}

case class BoolConst(value: Boolean) extends SimpleValue {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Bool)
}

case class MappingRef(mapName: String, key: Expression) extends Assignable {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = context.get(mapName) match {
    case None => Left(s"Undefined field $mapName")
    case Some(Mapping(keyType,valueType)) => key.getType(context) match {
      case Left(err) => Left(s"Type error in map key expression: $err")
      case Right(ty) => if (keyType == ty) {
        Right(valueType)
      } else {
        Left(s"Expected map key of type $keyType but found expression of type $ty")
      }
    }
    case Some(ty) => Left(s"Cannot perform key lookup on non-map type $ty")
  }
}

sealed trait LogicalOperator
case object Equal extends LogicalOperator
case object NotEqual extends LogicalOperator
case object LessThan extends LogicalOperator
case object LessThanOrEqual extends LogicalOperator
case object GreaterThan extends LogicalOperator
case object GreaterThanOrEqual extends LogicalOperator
case object And extends LogicalOperator
case object Or extends LogicalOperator

sealed trait ArithmeticOperator
case object Plus extends ArithmeticOperator
case object Minus extends ArithmeticOperator
case object Multiply extends ArithmeticOperator
case object Divide extends ArithmeticOperator

sealed trait Timespan extends SimpleValue {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Timespan)
}
case object Second extends Timespan
case object Minute extends Timespan
case object Hour extends Timespan
case object Day extends Timespan
case object Week extends Timespan

sealed trait Expression extends Typed
case class ValueExpression(value: SimpleValue) extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = value.getType(context)
}

case class LogicalExpression(left: Expression, operator: LogicalOperator, right: Expression) extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = for (
    leftTy <- left.getType(context);
    rightTy <- right.getType(context);

    resultTy <- operator match {
      case Equal | NotEqual =>
        if (leftTy != rightTy) Left(s"Cannot equals check $leftTy with $rightTy") else Right(Bool)
      case LessThan | LessThanOrEqual | GreaterThanOrEqual | GreaterThan =>
        if (leftTy != rightTy) Left(s"Cannot compare $leftTy with $rightTy")
        else leftTy match {
          case Int | Timestamp | Timespan => Right(Bool)
          case _ => Left(s"Cannot compare instances of unordered type $leftTy")
        }
      case And | Or => if (leftTy != Bool) Left(s"Cannot apply AND/OR to $leftTy")
                       else if (rightTy != Bool) Left(s"Cannot apply AND/OR to $rightTy")
                       else Right(Bool)
    }
  ) yield resultTy
}

case class ArithmeticExpression(left: Expression, operator: ArithmeticOperator, right: Expression) extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = for (
    leftTy <- left.getType(context);
    rightTy <- right.getType(context);
    resultTy <- leftTy match {
      case Int => operator match {
        case Multiply => rightTy match {
          case Int => Right(Int)
          case Timespan => Right(Timespan)
          case _ => Left(s"Cannot multiply int with $rightTy")
        }
        case _ => rightTy match {
          case Int => Right(Int)
          case _ => Left(s"Illegal operation between Int and $rightTy")
        }
      }

      case Timestamp => operator match {
        case Minus => rightTy match {
          case Timestamp => Right(Timespan)
          case _ => Left(s"Illegal operation between Timestamp and $rightTy")
        }
        case _ => Left("Illegal operation on Timestamp")
      }

      case Timespan => operator match {
        case Plus | Minus => rightTy match {
          case Timespan => Right(Timespan)
          case _ => Left(s"Illegal operation between Timespan and $rightTy")
        }
        case Multiply => rightTy match {
          case Int => Right(Timespan)
          case _ => Left(s"Illegal operation between Timespan and $rightTy")
        }
        case _ => Left(s"Illegal operation on Timespan")
      }

      case t => Left(s"Type $t does not support arithmetic")
    }
  ) yield resultTy
}

sealed trait AuthDecl {
  def extractIdentities: Set[String]
}

case class AuthValue(name: String) extends AuthDecl {
  override def extractIdentities: Set[String] = Set(name)
}

case class AuthCombination(left: AuthDecl, operator: LogicalOperator, right: AuthDecl) extends AuthDecl {
  override def extractIdentities: Set[String] = left.extractIdentities.union(right.extractIdentities)
}
