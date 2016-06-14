name := "sgenome"

version := "1.0"

scalaVersion := "2.11.8"

mainClass in (Compile,run) := Some("commandline.CommandLineEngine")
mainClass in assembly := Some("commandline.CommandLineEngine")

unmanagedJars in Compile += file("lib/htsjdk-2.4.1.jar")

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
