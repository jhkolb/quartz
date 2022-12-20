package edu.berkeley.cs.rise.quartz

object PayableExtractor {

  def extractPayableVars(stateMachine: StateMachine): (Set[String], Set[(String, String)], Set[(String, String)]) = {
    var fields: Set[String] = Set.empty[String]
    var params: Set[(String, String)] = Set.empty[(String, String)]
    val structFields: Set[(String, String)] = Set.empty[(String, String)]

    var previousSize = 0
    do {
      previousSize = fields.size + params.size + structFields.size
      stateMachine.transitions foreach { transition =>
        val transParams = transition.parameters.getOrElse(Seq.empty[Variable]).map(row => row.name)
        transition.body.getOrElse(Seq.empty[Statement]) foreach {
          case Send(destination, _, _) => {
            val destVar = identifyVariable(destination)
            if (transParams.contains(destVar)) {
              params += ((transition.name, destVar))
            } else {
              fields += destVar
            }
          }
          case Assignment(left, right) if fields.contains(left.rootName) ||
            params.contains((transition.name, left.rootName)) => {
            val rightVar = identifyVariable(right)
            if (transParams.contains(rightVar)) {
              params += ((transition.name, rightVar))
            } else {
              fields += rightVar
            }
          }
          case _ => "default"
        }
      }
    } while (previousSize != fields.size + params.size + structFields.size)

    println("Fields:", fields)
    println("Params:", params)
    println("Struct:", structFields)
    return (fields, params, structFields)
  }

  def identifyVariable(expression: Expression): String = expression match {
    case MappingRef(map, key) => identifyVariable(map)
    case VarRef(name) => name
    case SequenceSize(sequence) => identifyVariable(sequence)
    case _ => ""
  }
}
