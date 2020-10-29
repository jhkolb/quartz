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
      extractPayableFields(flattenStatements(stateMachine), stateMachine.fields.map(_.name).toSet)
    } while (previousSize != fields.size)

//    println("FIELDS: " + fields)

    // Determine Payable Params w/ Fix Point Iteration
    stateMachine.transitions foreach { transition => extractPayableParams(transition) }

//    println("PARAMS: " + params)

    previousSize = structFields.size
    do {
      previousSize = structFields.size
      extractPayableStruct(flattenStatements(stateMachine))
    } while (previousSize != structFields.size)

//    println("STRUCTS: " + structFields)

    return (fields, params, structFields)
  }

  private def extractPayableStruct(statements: Seq[Statement]): Unit = {
    structFields = statements.foldLeft(structFields) { (current, statement) =>
      statement match {
        case Send(destination, _, _) if destination.isInstanceOf[StructAccess] =>
          current.union(Set((getStructType(destination), destination.asInstanceOf[StructAccess].field)))
        case _ => current
      }
    }
  }

  private def getStructType(expression: Expression): String = expression match {
    // TODO: How to figure out data type of field for first case?
    // TODO: How to handle Mappings, Sequences
    case StructAccess(struct, _) if struct.isInstanceOf[StructAccess] => struct.asInstanceOf[StructAccess].field
    case StructAccess(struct, _) if struct.isInstanceOf[VarRef] =>
      struct.determinedType.toString.substring("Wrapper".length, struct.determinedType.toString.length-1)
    case _ => ""
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

  private def flattenStatements(stateMachine: StateMachine): Seq[Statement] = stateMachine.transitions.flatMap(_.body.getOrElse(Seq.empty[Statement]))
}