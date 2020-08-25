name := "quartz"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "com.github.scopt" %% "scopt" % "3.7.1"

mainClass in assembly := Some("edu.berkeley.cs.rise.quartz.Main")
assemblyJarName in assembly := "quartz.jar"
