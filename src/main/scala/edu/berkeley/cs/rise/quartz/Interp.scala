package edu.berkeley.cs.rise.quartz

import scala.annotation.tailrec

sealed trait QStatement
case object Top extends QStatement
case object Bottom extends QStatement
case class Interior(s: Statement) extends QStatement

sealed trait QuartzValue {
  def value: Any
}
case class QMapping(value: Map[QuartzValue, QuartzValue]) extends QuartzValue
case class QSequence(value: Seq[QuartzValue], elementType: DataType) extends QuartzValue
case class QStruct(value: Map[String, QuartzValue]) extends QuartzValue
case class QIdentity(value: String) extends QuartzValue
case class QBool(value: Boolean) extends QuartzValue
case class QHashValue(value: Seq[QuartzValue]) extends QuartzValue
case class QString(value: String) extends QuartzValue

sealed trait QuartzComparable extends QuartzValue {
  override def value: scala.Int
}
case class QInteger(value: scala.Int) extends QuartzComparable
case class QTimespan(value: scala.Int) extends QuartzComparable
case class QTimestamp(value: scala.Int) extends QuartzComparable

case class Message(sender: QIdentity, parameters: Map[String, QuartzValue])

object QuartzInterp {
  type InterpContext = Map[String, QuartzValue]
  type ApprovalMap = Map[(String, InterpContext), Set[QIdentity]]
  val AGENT_ZERO = "zero"

  def zeroElement(ty: DataType): QuartzValue = ty match {
    case _: Int => QInteger(0)
    case Timestamp => QTimestamp(0)
    case Mapping(_, _) => QMapping(Map.empty[QuartzValue, QuartzValue])
    case Sequence(elementType) => QSequence(Seq.empty[QuartzValue], elementType)
    case _: UnsignedInt => QInteger(0)
    case Struct(_) => QStruct(Map.empty[String, QuartzValue])
    case Identity => QIdentity(AGENT_ZERO)
    case String => QString("")
    case Bool => QBool(false)
    case HashValue(payloadTypes) => QHashValue(payloadTypes.map(zeroElement))
    case Timespan => QTimespan(0)
  }

  def evaluateExpression(expression: Expression, context: InterpContext): QuartzValue = expression match {
    case assignable: Assignable => assignable match {
      case VarRef(name) => context(name)
      case StructAccess(struct, field) =>
        val qStruct = evaluateExpression(struct, context).asInstanceOf[QStruct]
        qStruct.value(field)
      case MappingRef(map, key) =>
        val qMap = evaluateExpression(map, context).asInstanceOf[QMapping]
        val qKey = evaluateExpression(key, context)
        qMap.value(qKey)
    }

    case Second => QTimespan(1)
    case Minute => QTimespan(60)
    case Hour => QTimespan(60 * 60)
    case Day => QTimespan(24 * 60 * 60)
    case Week => QTimespan(7 * 24 * 60 * 60)

    case UnsignedIntConst(value) => QInteger(value)
    case IntConst(value) => QInteger(value)
    case StringLiteral(value) => QString(value)
    case BoolConst(value) => QBool(value)

    case SequenceSize(seq) =>
      val qSeq = evaluateExpression(seq, context).asInstanceOf[QSequence]
      QInteger(qSeq.value.length)

    case Hash(payload) => QHashValue(payload.map(evaluateExpression(_, context)))

    case LogicalOperation(left, operator, right) => operator match {
      case s: SequenceOperator =>
        val qElem = evaluateExpression(left, context)
        val qSeq = evaluateExpression(right, context).asInstanceOf[QSequence]
        s match {
          case In => QBool(qSeq.value.contains(qElem))
          case NotIn => QBool(!qSeq.value.contains(qElem))
        }

      case b: BooleanOperator =>
        val qLeft = evaluateExpression(left, context).asInstanceOf[QBool]
        val qRight = evaluateExpression(right, context).asInstanceOf[QBool]
        b match {
          case And => QBool(qLeft.value && qRight.value)
          case Or => QBool(qLeft.value || qRight.value)
          case Implies => throw new NotImplementedError("Implies Operator")
        }

      case c: Comparator =>
        val qLeft = evaluateExpression(left, context).asInstanceOf[QuartzComparable]
        val qRight = evaluateExpression(right, context).asInstanceOf[QuartzComparable]
        c match {
          case LessThanOrEqual => QBool(qLeft.value <= qRight.value)
          case Equal => QBool(qLeft.value == qRight.value)
          case NotEqual => QBool(qLeft.value != qRight.value)
          case GreaterThan => QBool(qLeft.value > qRight.value)
          case GreaterThanOrEqual => QBool(qLeft.value >= qRight.value)
          case LessThan => QBool(qLeft.value < qRight.value)
        }
    }

    // Much of this is simplified by assumptions we can make, as the AST must be well typed
    case ArithmeticOperation(left, operator, right) => evaluateExpression(left, context) match {
      case QInteger(leftValue) =>
        operator match {
          case Plus =>
            val rightValue = evaluateExpression(right, context).asInstanceOf[QInteger].value
            QInteger(leftValue + rightValue)
          case Minus =>
            val rightValue = evaluateExpression(right, context).asInstanceOf[QInteger].value
            QInteger(leftValue - rightValue)
          case Divide =>
            val rightValue = evaluateExpression(right, context).asInstanceOf[QInteger].value
            QInteger(leftValue / rightValue)
          case Modulo =>
            val rightValue = evaluateExpression(right, context).asInstanceOf[QInteger].value
            QInteger(leftValue % rightValue)
          case Multiply => evaluateExpression(right, context) match {
            case QInteger(rightValue) => QInteger(leftValue * rightValue)
            case QTimestamp(tValue) => QTimestamp(leftValue * tValue)
            case x => throw new IllegalArgumentException(s"Multiplying QInteger by $x")
          }
        }

      case QTimestamp(leftValue) => operator match {
        case Plus =>
          val rightValue = evaluateExpression(right, context).asInstanceOf[QTimespan].value
          QTimestamp(leftValue + rightValue)
        case Minus => evaluateExpression(right, context) match {
          case QTimestamp(rightValue) => QTimespan(leftValue - rightValue)
          case QTimespan(rightValue) => QTimestamp(leftValue - rightValue)
          case x => throw new IllegalArgumentException(s"Subtracting QTimestamp by $x")
        }
        case op => throw new IllegalArgumentException(s"Illegal operation on Timestamp: $op")
      }

      case QTimespan(leftValue) => operator match {
        case Plus =>
          val rightValue = evaluateExpression(right, context).asInstanceOf[QTimespan].value
          QTimespan(leftValue + rightValue)
        case Minus =>
          val rightValue = evaluateExpression(right, context).asInstanceOf[QTimespan].value
          QTimespan(leftValue - rightValue)
        case Multiply =>
          val rightValue = evaluateExpression(right, context).asInstanceOf[QInteger].value
          QTimespan(leftValue * rightValue)
        case op => throw new IllegalArgumentException(s"Illegal operation on Timespan: $op")
      }
      case x => throw new IllegalArgumentException(s"$x does not support arithmetic")
    }

    case _: LTLExpression => throw new NotImplementedError("LTL evaluation not implemented")
  }

  def evaluateAuthTerm(transition: String, authTerm: AuthTerm, parameters: InterpContext, context: InterpContext,
                       approvals: ApprovalMap): Boolean = authTerm match {
    case AuthAny(collection) =>
      val idSeq = evaluateExpression(collection, context).asInstanceOf[QSequence]
      assert(idSeq.elementType == Identity)
      val ids = idSeq.value.asInstanceOf[Seq[QIdentity]]
      ids.toSet.intersect(approvals.getOrElse((transition, parameters), Set.empty[QIdentity])).nonEmpty

    case AuthAll(collection) =>
      val idSeq = evaluateExpression(collection, context).asInstanceOf[QSequence]
      assert(idSeq.elementType == Identity)
      val ids = idSeq.value.asInstanceOf[Seq[QIdentity]]
      ids.toSet.diff(approvals.getOrElse((transition, parameters), Set.empty[QIdentity])).isEmpty

    case IdentityRef(identity) =>
      val id = evaluateExpression(identity, context).asInstanceOf[QIdentity]
      approvals.getOrElse((transition, parameters), Set.empty[QIdentity]).contains(id)
  }

  def evaluateStatement(statement: Statement, context: InterpContext): InterpContext = statement match {
    case SequenceAppend(sequence, element) =>
      val seqValue = evaluateExpression(sequence, context).asInstanceOf[QSequence]
      val elementValue = evaluateExpression(element, context)
      val replacementValue = QSequence(seqValue.value :+ elementValue, seqValue.elementType)
      val target = rebuildTarget(sequence, replacementValue, context)
      context + (sequence.rootName -> target)

    case Assignment(left, right) =>
      val rightValue = evaluateExpression(right, context)
      val target = rebuildTarget(left, rightValue, context)
      context + (left.rootName -> target)

    case SequenceClear(sequence) =>
      val seqValue = evaluateExpression(sequence, context).asInstanceOf[QSequence]
      val replacementValue = QSequence(Seq.empty[QuartzValue], seqValue.elementType)
      val target = rebuildTarget(sequence, replacementValue, context)
      context + (sequence.rootName -> target)

    case Send(_, _, _) => throw new NotImplementedError("Evaluate send")
  }

  // Recursive, rebuild QuartzValue as you go along
  // Needed to simulate mutable values in Quartz execution
  @tailrec
  private def rebuildTarget(target: Assignable, newValue: QuartzValue, context: InterpContext): QuartzValue = target match {
    case VarRef(_) => newValue
    case StructAccess(struct, field) =>
      val structValue = evaluateExpression(struct, context).asInstanceOf[QStruct]
      val result = QStruct(structValue.value + (field -> newValue))
      rebuildTarget(struct, result, context)
    case MappingRef(map, key) =>
      val mapValue = evaluateExpression(map, context).asInstanceOf[QMapping]
      val keyValue = evaluateExpression(key, context)
      val result = QMapping(mapValue.value + (keyValue -> newValue))
      rebuildTarget(map, result, context)
  }
}
