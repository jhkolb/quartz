package edu.berkeley.cs.rise.quartz

object PayableExtractor {

  private val BUILTIN_PARAMS: Set[String] = Set("tokens")

  private var fields: Set[String] = Set.empty[String]
  private var params: Set[(String, String)] = Set.empty[(String, String)]
  private var structFields: Set[(String, String)] = Set.empty[(String, String)]

  def extractPayableVars(stateMachine: StateMachine): (Set[String], Set[(String, String)], Set[(String, String)]) = {
    // Determine Payable Fields w/ Fix Point Iteration
    var previousSize = fields.size
    do {
      previousSize = fields.size
      extractPayableFields(stateMachine.flattenStatements, stateMachine.fields.map(_.name).toSet)
    } while (previousSize != fields.size)

    println("FIELDS: " + fields)

    // Determine Payable Params w/ Fix Point Iteration
    stateMachine.transitions foreach { transition => extractPayableParams(transition) }

    println("PARAMS: " + params)

    return (fields, params, structFields)
  }

  private def extractPayableParams(transition: Transition) = {
    var previousSize = params.size
    do {
      previousSize = params.size
      params = transition.body.getOrElse(Seq.empty[Statement]).foldLeft(params) { (current, statement) =>
        statement match {
          case Send(destination, _, _) => current.union(Set((transition.name, extractHelper(destination))))
          case Assignment(left, right) if current.contains((transition.name, left.rootName)) || fields.contains(left.rootName) => current.union(Set((transition.name, extractHelper(right))))
          case _ => current
        }
      }
    } while (params.size != previousSize)
  }

  private def extractPayableFields(statements: Seq[Statement], scope: Set[String]) = {
    val names = statements.foldLeft(fields) { (current, statement) =>
      statement match {
        case Send(destination, _, _) => current.union(Set(extractHelper(destination)))
        case Assignment(left, right) if current.contains(left.rootName) => current.union(Set(extractHelper(right)))
        case _ => current
      }
    }
    if (scope.nonEmpty) {
      fields = names.intersect(scope)
    } else {
      fields = names
    }
  }

  private def extractHelper(expression: Expression): String = expression match {
    case MappingRef(map, key) => extractHelper(map)
    case VarRef(name) => name
    case LogicalOperation(left, _, right) => extractHelper(left) ++ extractHelper(right)
    case ArithmeticOperation(left, _, right) => extractHelper(left) ++ extractHelper(right)
    case SequenceSize(sequence) => extractHelper(sequence)
    case _ => ""
  }
}