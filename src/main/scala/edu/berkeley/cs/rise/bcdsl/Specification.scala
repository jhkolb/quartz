package edu.berkeley.cs.rise.bcdsl

case class Specification(name: String, stateMachine: StateMachine, invariants: Option[Seq[LTLProperty]]) {
  @scala.annotation.tailrec
  private def validateLTLProperty(ltlProp: LTLProperty, context: Map[String, DataType]): Option[String] = ltlProp match {
    case LTLProperty(_, Left(p)) => validateLTLProperty(p, context)
    case LTLProperty(_, Right(exp)) => exp.getType(context) match {
      case Left(msg) => Some(msg)
      case Right(_) => None
    }
  }

  // For now, this just verifies that all referenced variables are well defined
  private def validateLTLProperties(): Option[String] = {
    // State names are treated as booleans. Each is true if the machine is in that state.
    val baseMap = Specification.RESERVED_VALUES ++ stateMachine.states.map(_ -> Bool).toMap
    val fieldMap = stateMachine.fields.foldLeft(baseMap){ (ctx, f) => ctx + (f.name -> f.ty) }
    val ltlContext = stateMachine.transitions.foldLeft(fieldMap) { (ctx, t) =>
      t.parameters.fold(ctx)(params => ctx ++ params.map(p => s"${t.name}.${p.name}" -> p.ty).toMap)
    }
    invariants.foreach(_.foreach(validateLTLProperty(_, ltlContext) match {
      case None => Unit
      case err => return err
    }))

    None
  }

  def validate(): Option[String] = {
    stateMachine.validate() match {
      case None => validateLTLProperties()
      case e @ Some(_) => e
    }
  }
}

object Specification {
  // Special keywords that cannot appear as ordinary variables
  val RESERVED_VALUES: Map[String, DataType] = Map[String, DataType](
    "balance" -> Int,
    "now" -> Timestamp,
    "sender" -> Identity,
  )

  // Special transition parameters that must have a specific type if used
  val CONSTRAINED_PARAMS: Map[String, DataType] = Map[String, DataType](
    "tokens" -> Int,
  )
}
