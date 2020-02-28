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
case class Struct(name: String) extends DataType

case class Context(structs: Map[String, Map[String, DataType]], variables: Map[String, DataType])

sealed abstract class Typed {
  private var ty: Option[DataType] = None

  protected def determineType(context: Context): Either[String, DataType]

  def getType(context: Context): Either[String, DataType] = {
    determineType(context) match {
      case result@Right(determinedTy) =>
        ty = Some(determinedTy)
        result
      case err => err
    }
  }

  // Convenience method for later stages of translation
  // Should only be used *after* successful type checking
  def determinedType: DataType = ty match {
    case None => throw new IllegalStateException("Expression's type is still undetermined")
    case Some(determinedTy) => determinedTy
  }
}

sealed trait Expression extends Typed

sealed trait Assignable extends Expression

case class VarRef(name: String) extends Assignable {
  override def determineType(context: Context): Either[String, DataType] = context.variables.get(name) match {
    case None => Left(s"Undefined field $name")
    case Some(ty) => Right(ty)
  }
}

case class IntConst(value: Int) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = Right(Int)
}

case class StringLiteral(value: String) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = Right(String)
}

case class BoolConst(value: Boolean) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = Right(Bool)
}

case class MappingRef(map: Expression, key: Expression) extends Assignable {
  override def determineType(context: Context): Either[String, DataType] = map.getType(context) match {
    case Left(msg) => Left(s"Invalid mapping type: $msg")
    case Right(Mapping(keyType, valueType)) => key.getType(context) match {
      case Left(err) => Left(s"Type error in map key expression: $err")
      case Right(ty) => if (keyType == ty) {
        Right(valueType)
      } else {
        Left(s"Expected map key of type $keyType but found expression of type $ty")
      }
    }
    case Right(ty) => Left(s"Cannot perform key lookup on non-map type $ty")
  }

  // Used to extract name of outermost mapping, e.g. (m[x][y][z]).rootName is "m"
  // Useful for type checking struct field accesses (e.g. "s.m[94][95]")
  lazy val rootName: String = map match {
    case VarRef(name) => name
    case m@MappingRef(_, _) => m.rootName
    case _ => throw new IllegalArgumentException("Invalid MappingRef")
  }
}

case class StructAccess(struct: Assignable, field: Assignable) extends Assignable {
  override protected def determineType(context: Context): Either[String, DataType] = {
    struct.getType(context) match {
      case Left(msg) => Left(s"Invalid struct type: $msg")
      case Right(Struct(structName)) => context.structs.get(structName) match {
        case None => Left(s"Struct type $structName undefined")
        case Some(structFields) => field match {
          case VarRef(name) => structFields.get(name) match {
            case None => Left(s"Struct type $structName has no field $name")
            case Some(ty) => Right(ty)
          }

          case m@MappingRef(_, _) => structFields.get(m.rootName) match {
            case None => Left(s"Struct type $structName has no field ${m.rootName}")
            // Note that if the struct contains a field name already defined in the original context,
            // the field's name-type binding will have higher precedence
            case Some(_) =>
              val augmentedVars = context.variables ++ structFields
              m.getType(Context(context.structs, augmentedVars))
          }

          // This should never happen as the "." operator is left associative
          case _: StructAccess => throw new IllegalArgumentException("Malformed struct reference")
        }
      }
      case Right(ty) => Left(s"Cannot access field of non struct type $ty")
    }
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
  override def determineType(context: Context): Either[String, DataType] = Right(Timespan)
}
case object Second extends Timespan
case object Minute extends Timespan
case object Hour extends Timespan
case object Day extends Timespan
case object Week extends Timespan

case class LogicalOperation(left: Expression, operator: LogicalOperator, right: Expression) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = for (
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
  override def determineType(context: Context): Either[String, DataType] = for (
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
  override def determineType(context: Context): Either[String, DataType] =
    sequence.getType(context) match {
      case err@Left(_) => err
      case Right(Sequence(_)) => Right(Int)
      case Right(ty) => Left(s"Cannot compute size of non-sequence type $ty")
    }
}

case class LTLProperty(operator: LTLOperator, body: Either[LTLProperty, Expression])

sealed trait AuthExpression {
  private[bcdsl] def flatten: Set[AuthTerm]
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
case class SequenceClear(sequence: Expression) extends Statement
