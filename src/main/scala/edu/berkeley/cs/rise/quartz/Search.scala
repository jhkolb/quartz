package edu.berkeley.cs.rise.quartz

import edu.berkeley.cs.rise.quartz.QuartzInterp.{ApprovalMap, InterpContext}

import scala.collection.mutable

case class Status(currentState: String, context: InterpContext, approvals: ApprovalMap,
                  messages: Seq[Message], statement: Statement, auth: Option[AuthTerm])

case class Domain(minInt: QInteger, maxInt: QInteger, minTimestamp: QTimestamp, maxTimestamp: QTimestamp,
                  minTimespan: QTimespan, maxTimespan: QTimespan, numIdentities: scala.Int) {

  def getDomain(dataType: DataType): Set[QuartzValue] = dataType match {
    case _: Int => (minInt.value to maxInt.value).map(i => QInteger(i)).toSet
    case _: UnsignedInt => (0 to maxInt.value).map(i => QInteger(i)).toSet
    case Timestamp => (minTimestamp.value to maxTimestamp.value).map(i => QTimestamp(i)).toSet
    case Timespan => (minTimespan.value to maxTimespan.value).map(i => QTimespan(i)).toSet
    case Identity => (1 to numIdentities).map(i => QIdentity(s"id_$i")).toSet
    case Bool => Set(QBool(false), QBool(true))
    case String => Set("PLACEHOLDER")
    case HashValue(payloadTypes) => payloadTypes match {
      case Nil => Set(QHashValue(Nil))
      case t::ts => (for {
        hashVal <- getDomain(HashValue(ts)).asInstanceOf[Set[QHashValue]]
        elem <- getDomain(t)
      } yield QHashValue(elem +: hashVal.value)).toSet
    }

    case Mapping(keyType, valueType) => throw new NotImplementedError()
    case Sequence(elementType) => throw new NotImplementedError()
    case Struct(name) => throw new NotImplementedError()
    case HashValue(payloadTypes) => throw new NotImplementedError()
  }

  def enumerateMessages(paramTypes: Seq[Variable]): Set[Map[String, QuartzValue]] = throw new NotImplementedError
}

object Search {
  def bfs(spec: Specification, domain: Domain): Option[String] = {
    val initialTransition = spec.stateMachine.transitions.filter(_.origin.isEmpty).head
    val initialContext = spec.stateMachine.fields.map { case Variable(name, ty) => name -> QuartzInterp.zeroElement(ty) }.toMap
    val queue = mutable.Queue.empty[Status]
    None
  }
}
