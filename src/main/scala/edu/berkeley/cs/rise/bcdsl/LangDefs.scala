package edu.berkeley.cs.rise.bcdsl

sealed trait DataType
case object Identity extends DataType
case object Int extends DataType
case object String extends DataType
case object Timestamp extends DataType
case object Bool extends DataType
case object Timespan extends DataType
case class Mapping(keyType: DataType, valueType: DataType) extends DataType
case class Sequence(elementType: DataType) extends DataType

trait Typed {
  def getType(context: Map[String, DataType]): Either[String, DataType]
}

sealed trait Expression extends Typed
sealed trait Assignable extends Expression

case class VarRef(name: String) extends Assignable {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = context.get(name) match {
    case None => Left(s"Undefined field $name")
    case Some(ty) => Right(ty)
  }
}

case class IntConst(value: Int) extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Int)
}

case class StringLiteral(value: String) extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(String)
}

case class BoolConst(value: Boolean) extends Expression {
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

sealed trait Comparator extends LogicalOperator
case object Equal extends Comparator
case object NotEqual extends Comparator
case object LessThan extends Comparator
case object LessThanOrEqual extends Comparator
case object GreaterThan extends Comparator
case object GreaterThanOrEqual extends Comparator

sealed trait BooleanOperator extends LogicalOperator
case object And extends BooleanOperator
case object Or extends BooleanOperator

sealed trait SequenceOperator extends LogicalOperator
case object In extends SequenceOperator
case object NotIn extends SequenceOperator

sealed trait ArithmeticOperator
case object Plus extends ArithmeticOperator
case object Minus extends ArithmeticOperator
case object Multiply extends ArithmeticOperator
case object Divide extends ArithmeticOperator

sealed trait LTLOperator
case object Always extends LTLOperator
case object Eventually extends LTLOperator

sealed trait Timespan extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] = Right(Timespan)
}
case object Second extends Timespan
case object Minute extends Timespan
case object Hour extends Timespan
case object Day extends Timespan
case object Week extends Timespan

case class LogicalOperation(left: Expression, operator: LogicalOperator, right: Expression) extends Expression {
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

      case In | NotIn => rightTy match {
        case Sequence(elementType) => leftTy match {
          case ty if ty == elementType => Right(Bool)
          case ty => Left(s"Cannot check element of type $ty for membership in sequence of $elementType instances")
        }
        case ty => Left(s"Cannot check membership of non-sequence type $ty")
      }
    }
  ) yield resultTy
}

case class ArithmeticOperation(left: Expression, operator: ArithmeticOperator, right: Expression) extends Expression {
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

case class SequenceSize(sequence: Expression) extends Expression {
  override def getType(context: Map[String, DataType]): Either[String, DataType] =
    sequence.getType(context) match {
      case err @ Left(_) => err
      case Right(Sequence(_)) => Right(Int)
      case Right(ty) => Left(s"Cannot compute size of non-sequence type $ty")
    }
}

case class LTLProperty(operator: LTLOperator, body: Either[LTLProperty, Expression])

sealed trait AuthExpression {
  private [bcdsl] def flatten: Set[AuthTerm]
}

sealed trait AuthTerm extends AuthExpression {
  def getReferencedName: String
}

case class IdentityLiteral(identity: String) extends AuthTerm {
  override def flatten: Set[AuthTerm] = Set(this)

  override def getReferencedName: String = identity
}

case class AuthAny(collectionName: String) extends AuthTerm {
  override def flatten: Set[AuthTerm] = Set(this)

  override def getReferencedName: String = collectionName
}

case class AuthAll(collectionName: String) extends AuthTerm {
  override def flatten: Set[AuthTerm] = Set(this)

  override def getReferencedName: String = collectionName
}

case class AuthCombination(left: AuthExpression, operator: BooleanOperator, right: AuthExpression) extends AuthExpression {
  override def flatten: Set[AuthTerm] = left.flatten.union(right.flatten)
}

sealed trait Statement
case class Assignment(left: Assignable, right: Expression) extends Statement
case class Send(destination: Expression, amount: Expression, source: Option[Assignable]) extends Statement
case class SequenceAppend(sequence: Expression, element: Expression) extends Statement
