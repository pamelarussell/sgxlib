name := "sgenome"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile,run) := Some("Hello")

unmanagedJars in Compile += file("lib/htsjdk-2.4.1.jar")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
