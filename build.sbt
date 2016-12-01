val nm = "sgxlib"
val ver = "1.0"

name := nm
version := ver
scalaVersion := "2.11.8"
unmanagedJars in Compile += file("lib/htsjdk-2.4.1.jar")
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
testOptions in Test += Tests.Argument("-oF")
assemblyJarName in assembly := nm + "-" + ver + ".jar"
coverageEnabled := true

import org.scoverage.coveralls.Imports.CoverallsKeys._
coverallsTokenFile := Some("src/main/resources/token.txt")
