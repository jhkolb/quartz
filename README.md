# quartz

Summary of source code files:
  1. `LangDefs.scala`: Defines all language structures: expressions, statements, types, etc. Also contains type-checking logic.
  2. `StateMachine.scala`: Defines a Quartz state machine, consisting of states, fields, and transitions. Also defines logic to validate that a state machine is properly structured.
  3. `Specification.scala`: Defines a Quartz specification, i.e., a `StateMachine` instance plus optional properties defined by the user that they would like verified.
  4. `SpecificationParser.scala`: Parses text into a `Specification` instance using Scala's combinator parsing library.
  5. `Solidity.scala`: Defines a translator from a `Specification` to equivalent Solidity source code.
  6. `PlusCal.scala`: Defines a translator from a `Specification` to partially complete PlusCal code.
  7. `TLA.scala`: Generates auxiliary files (configuration, external definitions) required by TLA+. Converts generated PlusCal code into a complete TLA+ specification.
  8. `Main.scala`: Executed when Quartz is invoked from the command line. Dispatches to appropriate translator and writes to output file(s).
  9. `ArgumentParser.scala`: Parses the flags used when Quartz is invoked from the command line.
  10. `Utils.scala`: Miscellaneous utility functions for use in other files.
  
  There are also many sample Quartz contracts defined in `src/test/resources`. Note that not all of these contracts are well-formed, as some are used in unit tests to verify that we properly identify errors.
