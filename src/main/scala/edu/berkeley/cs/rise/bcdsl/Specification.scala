package edu.berkeley.cs.rise.bcdsl

case class Specification(name: String, stateMachine: StateMachine, invariants: Option[Seq[LTLProperty]]) {
  // For now, this just verifies that all referenced variables are well defined
  @scala.annotation.tailrec
  private def validateLTLProperty(ltlProp: LTLProperty, context: Context): Option[String] = ltlProp match {
    case LTLProperty(_, Left(p)) => validateLTLProperty(p, context)
    case LTLProperty(_, Right(exp)) => exp.getType(context) match {
      case Left(msg) => Some(msg)
      case Right(_) => None
    }
  }

  private def validateLTLProperties(): Option[String] = {
    // State names are treated as booleans. Each is true if the machine is in that state.
    val baseMap = Specification.RESERVED_VALUES ++ stateMachine.states.map(_ -> Bool).toMap
    val fieldMap = stateMachine.fields.foldLeft(baseMap){ (ctx, f) => ctx + (f.name -> f.ty) }

    // Each transition implicitly defines a struct with all of the its parameters as fields
    // These structs can be referenced from LTL properties
    // TLA translation handles defining them for use in model checking
    val transitionStructs = stateMachine.transitions.map { t =>
      val structName = s"__${t.name}Struct"
      val structFields = t.parameters.fold(Map.empty[String, DataType])(_.map(p => p.name -> p.ty).toMap)
      structName -> structFields
    }.toMap
    val transitionVars = stateMachine.transitions.map(t => t.name -> Struct(s"__${t.name}Struct")).toMap
    val ltlContext = Context(transitionStructs, fieldMap ++ transitionVars)

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
    "balance" -> UnsignedIntVar,
    "now" -> Timestamp,
    "sender" -> Identity,
  )

  // Special transition parameters that must have a specific type if used
  val CONSTRAINED_PARAMS: Map[String, DataType] = Map[String, DataType](
    "tokens" -> UnsignedIntVar,
  )

  private def extractMaxMinTargets(expression: Expression): (Set[Assignable], Set[Assignable]) = expression match {
    case LTLMax(body) => (Set(body), Set.empty[Assignable])
    case LTLMin(body) => (Set.empty[Assignable], Set(body))

    case LogicalOperation(left, _, right) =>
      val (leftMax, leftMin) = extractMaxMinTargets(left)
      val (rightMax, rightMin) = extractMaxMinTargets(right)
      (leftMax.union(rightMax), leftMin.union(rightMin))

    case ArithmeticOperation(left, _, right) =>
      val (leftMax, leftMin) = extractMaxMinTargets(left)
      val (rightMax, rightMin) = extractMaxMinTargets(right)
      (leftMax.union(rightMax), leftMin.union(rightMin))

    case _ => (Set.empty[Assignable], Set.empty[Assignable])
  }

  @scala.annotation.tailrec
  private[bcdsl] def extractMaxMinTargets(prop: LTLProperty): (Set[Assignable], Set[Assignable]) = prop match {
    case LTLProperty(_, Left(nestedProp)) => extractMaxMinTargets(nestedProp)
    case LTLProperty(_, Right(exp)) => extractMaxMinTargets(exp)
  }
}
