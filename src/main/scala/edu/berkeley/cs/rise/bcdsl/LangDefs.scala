package edu.berkeley.cs.rise.bcdsl

sealed trait DataType
case object Identity extends DataType
case object String extends DataType
case object Timestamp extends DataType
case object Bool extends DataType
case object Timespan extends DataType

sealed trait Int extends DataType
case object IntVar extends Int
case object IntConst extends Int

sealed trait UnsignedInt extends DataType
case object UnsignedIntVar extends UnsignedInt
case object UnsignedIntConst extends UnsignedInt

case class HashValue(payloadTypes: Seq[DataType]) extends DataType {
  def isCompatibleWith(other: HashValue): Boolean = this.payloadTypes.length == other.payloadTypes.length &&
    this.payloadTypes.zip(other.payloadTypes).forall { case (left, right) =>
      left match {
        case IntVar | IntConst => right match {
          case IntVar | IntConst | UnsignedIntConst => true
          case _ => false
        }

        case UnsignedIntVar | UnsignedIntConst => right match {
          case UnsignedIntVar | UnsignedIntConst => true
          case _ => false
        }

        case l@HashValue(_) => right match {
          case r@HashValue(_) => l.isCompatibleWith(r)
          case _ => false
        }

        case Timestamp | Timespan | Identity | Bool | Sequence(_) | String | Struct(_) => left == right

        case _ => false
      }
    }

  override def toString: String = s"HashValue[${payloadTypes.mkString(", ")}]"
}

case class Mapping(keyType: DataType, valueType: DataType) extends DataType {
  override def toString: String = s"Mapping[$keyType, $valueType]"
}

case class Sequence(elementType: DataType) extends DataType {
  override def toString: String = s"Sequence[$elementType]"
}

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

case class IntConst(value: scala.Int) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = Right(IntConst)
}

case class UnsignedIntConst(value: scala.Int) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = {
    assert(value >= 0)
    Right(UnsignedIntConst)
  }
}

case class StringLiteral(value: String) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = Right(String)
}

case class BoolConst(value: Boolean) extends Expression {
  override def determineType(context: Context): Either[String, DataType] = Right(Bool)
}

case class Hash(payload: Seq[Expression]) extends Expression {
  override protected def determineType(context: Context): Either[String, DataType] =
    // Determine type of each payload element, return first error encountered
    // If no type errors, yield a list of data types
    // Probably a fancy functional way to do this with the equivalent of Haskell's `sequence`
    payload.map(_.getType(context)).foldLeft(Right(Seq.empty[DataType]): Either[String, Seq[DataType]]) {
      case (previous, current) =>
        previous match {
          case l@Left(_) => l
          case Right(prevTypes) => current match {
            case l@Left(_) => l.asInstanceOf[Either[String, Seq[DataType]]]
            case Right(ty) => Right(prevTypes :+ ty)
          }
        }
    } match {
      case l@Left(_) => l.asInstanceOf[Either[String, DataType]]
      case Right(payloadTypes) => Right(HashValue(payloadTypes))
    }
}


sealed trait Assignable extends Expression {
  def rootName: String
}

case class VarRef(name: String) extends Assignable {
  override def determineType(context: Context): Either[String, DataType] = context.variables.get(name) match {
    case None => Left(s"Undefined variable $name")
    case Some(ty) => Right(ty)
  }

  override val rootName: String = name
}

case class MappingRef(map: Expression, key: Expression) extends Assignable {
  override def determineType(context: Context): Either[String, DataType] = map.getType(context) match {
    case Left(msg) => Left(s"Invalid mapping type: $msg")

    case Right(Mapping(keyType, valueType)) => key.getType(context) match {
      case Left(err) => Left(s"Type error in map key expression: $err")

      case Right(IntVar) | Right(IntConst) => keyType match {
        case IntVar => Right(valueType)
        case _ => Left(s"Expected map key of type $keyType but found expression of type Int")
      }

      case Right(UnsignedIntConst) => keyType match {
        case IntVar | UnsignedIntVar => Right(valueType)
        case _ => Left(s"Expected map key of type $keyType but found expression of type unsigned int")
      }

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
  // And for generating authorization variable names
  override lazy val rootName: String = map match {
    case VarRef(name) => name
    case m: MappingRef => m.rootName
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

  override lazy val rootName: String = struct match {
    case VarRef(name) => name
    case m: MappingRef => m.rootName
    case s: StructAccess => s.rootName
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
      case Equal | NotEqual => leftTy match {
        case IntVar | IntConst => rightTy match {
          case IntVar | IntConst | UnsignedIntConst => Right(Bool)
          case _ => Left(s"Cannot compare $leftTy with $rightTy")
        }

        case UnsignedIntVar | UnsignedIntConst => rightTy match {
          case UnsignedIntVar | UnsignedIntConst => Right(Bool)
          case _ => Left(s"Cannot compare $leftTy with $rightTy")
        }

        case leftHash@HashValue(_) => rightTy match {
          case rightHash@HashValue(_) => if (leftHash.isCompatibleWith(rightHash)) {
            Right(Bool)
          } else {
            Left(s"Cannot compare $leftTy with $rightTy")
          }

          case _ => Left(s"Cannot compare $leftTy with $rightTy")
        }

        case Timestamp | Timespan | Identity | Bool | Sequence(_) | String | Struct(_) =>
          if (leftTy != rightTy) Left(s"Cannot compare $leftTy with $rightTy") else Right(Bool)

        case _ => Left(s"Cannot determine equality for instances of $leftTy")
      }

      case LessThan | LessThanOrEqual | GreaterThanOrEqual | GreaterThan => leftTy match {
        case IntConst | IntVar => rightTy match {
          case IntConst | IntVar| UnsignedIntConst | UnsignedIntVar => Right(Bool)
          case _ => Left(s"Cannot compare instances of $leftTy and $rightTy")
        }

        case UnsignedIntConst | UnsignedIntVar => rightTy match {
          case UnsignedIntConst | UnsignedIntVar => Right(Bool)
          case _ => Left(s"Cannot compare instances of $leftTy and $rightTy")
        }

        case Timestamp | Timespan =>
          if (leftTy != rightTy) Left(s"Cannot compare $leftTy with $rightTy") else Right(Bool)

        case _ => Left(s"Cannot compare instances of unordered type $leftTy")
      }

      case And | Or => leftTy match {
        case Bool => rightTy match {
          case Bool => Right(Bool)
          case _ => Left(s"Cannot apply AND/OR to $rightTy")
        }
        case _ => Left(s"Cannot apply AND/OR to $leftTy")
      }

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
      case IntConst => operator match {
        case Multiply => rightTy match {
          case IntConst => Right(IntConst)
          case IntVar => Right(IntVar)
          case Timespan => Right(Timespan)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }
        case _ => rightTy match {
          case IntConst => Right(IntConst)
          case IntVar => Right(IntVar)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }
      }

      case IntVar => operator match {
        case Multiply => rightTy match {
          case IntConst | IntVar => Right(IntVar)
          case Timespan => Right(Timespan)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }

        case _ => rightTy match {
          case IntConst | IntVar => Right(IntVar)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }
      }

      case UnsignedIntConst => operator match {
        case Multiply => rightTy match {
          case UnsignedIntConst => Right(UnsignedIntConst)
          case UnsignedIntVar => Right(UnsignedIntVar)
          case Timespan => Right(Timespan)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }
        case _ => rightTy match {
          case UnsignedIntConst => Right(UnsignedIntConst)
          case UnsignedIntVar => Right(UnsignedIntVar)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }
      }

      case UnsignedIntVar => operator match {
        case Multiply => rightTy match {
          case UnsignedIntConst | UnsignedIntVar => Right(UnsignedIntVar)
          case Timespan => Right(Timespan)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }

        case _ => rightTy match {
          case UnsignedIntConst | UnsignedIntVar => Right(UnsignedIntVar)
          case _ => Left(s"Illegal operation between $leftTy and $rightTy")
        }
      }

      case Timestamp => operator match {
        case Minus => rightTy match {
          case Timespan => Right(Timestamp)
          case Timestamp => Right(Timespan)
          case _ => Left(s"Illegal subtraction of $rightTy from timestamp")
        }

        case Plus => rightTy match {
          case Timespan => Right(Timestamp)
          case _ => Left(s"Illegal addition of $rightTy to timestamp")
        }

        case Multiply | Divide => Left(s"Cannot multiply or divide timestamps")
      }

      case Timespan => operator match {
        case Plus | Minus => rightTy match {
          case Timespan => Right(Timespan)
          case _ => Left(s"Illegal operation between Timespan and $rightTy")
        }
        case Multiply => rightTy match {
          case IntVar | IntConst | UnsignedIntVar | UnsignedIntConst => Right(Timespan)
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
      case Right(Sequence(_)) => Right(UnsignedIntConst)
      case Right(ty) => Left(s"Cannot compute size of non-sequence type $ty")
    }
}

case class LTLProperty(operator: LTLOperator, body: Either[LTLProperty, Expression])

sealed trait AuthExpression {
  def basis: Set[AuthTerm]
  def validate(context: Context): Option[String]
}

sealed trait AuthTerm extends AuthExpression {
  def referencedAssignable: Assignable
}

case class IdentityRef(identity: Assignable) extends AuthTerm {
  override val basis = Set(this)
  override val referencedAssignable: Assignable = identity

  def validate(context: Context): Option[String] = identity.getType(context) match {
    case Left(err) => Some(err)
    case Right(Identity) => None
    case Right(t) => Some(s"Authorization refers to non-identity type $t")
  }
}

case class AuthAny(collection: Assignable) extends AuthTerm {
  override val basis = Set(this)
  override val referencedAssignable: Assignable = collection

  override def validate(context: Context): Option[String] = collection.getType(context) match {
    case Left(err) => Some(err)
    case Right(Sequence(Identity)) => None
    case Right(t) => Some(s"Cannot use instance of type $t in `Any` term")
  }
}

case class AuthAll(collection: Assignable) extends AuthTerm {
  override val basis = Set(this)
  override val referencedAssignable: Assignable = collection

  override def validate(context: Context): Option[String] = collection.getType(context) match {
    case Left(err) => Some(err)
    case Right(Sequence(Identity)) => None
    case Right(t) => Some(s"Cannot use instance of type $t in `All` term")
  }
}

case class AuthCombination(left: AuthExpression, operator: BooleanOperator, right: AuthExpression) extends AuthExpression {
  override val basis: Set[AuthTerm] = left.basis.union(right.basis)

  override def validate(context: Context): Option[String] = left.validate(context) match {
    case None => right.validate(context)
    case s => s
  }
}

sealed trait Statement {
  def validate(context: Context): Option[String]
}

case class Assignment(left: Assignable, right: Expression) extends Statement {
  override def validate(context: Context): Option[String] = {
    val result = for (
      leftTy <- left.getType(context);
      rightTy <- right.getType(context);
      res <- leftTy match {
        case IntVar => rightTy match {
          case IntVar | IntConst | UnsignedIntConst => Right(Unit)
          case _ => Left(s"Cannot assign instance of $rightTy to $leftTy")
        }

        case UnsignedIntVar => rightTy match {
          case UnsignedIntVar | UnsignedIntConst => Right(Unit)
          case _ => Left(s"Cannot assign instance of $rightTy to $leftTy")
        }

        case IntConst | UnsignedIntConst => Left(s"Cannot assign value to integer constant")

        case _ => if (leftTy != rightTy) Left(s"Cannot assign instance of $rightTy to $leftTy") else Right(Unit)
      }
    ) yield res

    result.left.toOption
  }
}

case class Send(destination: Expression, amount: Expression, source: Option[Assignable]) extends Statement {
  override def validate(context: Context): Option[String] = {
    val result = for (
      destTy <- destination.getType(context);
      amountTy <- amount.getType(context);
      sourceTy <- source.fold(Right(UnsignedIntVar): Either[String, DataType])(_.getType(context))
    ) yield destTy match {
      case Identity => amountTy match {
        case UnsignedIntVar | UnsignedIntConst => sourceTy match {
          case UnsignedIntVar => Right(Unit)
          case _ => Left(s"Expected send source of unsigned integer var type but found $sourceTy")
        }
        case _ => Left(s"Expected send amount of unsigned integer type but found $amountTy")
      }
      case _ => Left(s"Expected send destination of Identity type but found $destTy")
    }

    result.left.toOption
  }
}

case class SequenceAppend(sequence: Expression, element: Expression) extends Statement {
  override def validate(context: Context): Option[String] = {
    val result = for (
      sequenceTy <- sequence.getType(context);
      elementTy <- element.getType(context)
    ) yield sequenceTy match {
      case Sequence(t) =>
        if (t == elementTy) Right(Unit)
        else Left(s"Cannot append instance of $elementTy to sequence of $t instances")
      case t => Left(s"Cannot append to non-sequence type $t")
    }

    result.left.toOption
  }
}

case class SequenceClear(sequence: Expression) extends Statement {
  override def validate(context: Context): Option[String] = sequence.getType(context) match {
    case Left(err) => Some(err)
    case Right(Sequence(_)) => None
    case Right(t) => Some(s"Cannot clean non sequence type $t")
  }
}
