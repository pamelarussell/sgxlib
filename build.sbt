val nm = "sgxlib"
val ver = "1.0.0"

// Basic info
name := nm
version := ver
scalaVersion := "2.11.8"

// sbt assembly to generate fat .jar
assemblyJarName in assembly := nm + "-" + ver + ".jar"

// Scalatest
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
testOptions in Test += Tests.Argument("-oF")

// Coveralls
import org.scoverage.coveralls.Imports.CoverallsKeys._
coverallsTokenFile := Some("src/main/resources/token.txt")
coverageEnabled := true

// htsjdk
unmanagedJars in Compile += file("lib/htsjdk-2.4.1.jar")
apiMappings += (
  (unmanagedBase.value / "lib/htsjdk-2.4.1.jar") ->
    url("http://samtools.github.io/htsjdk/javadoc/htsjdk/")
  )

// Scaladoc
target in Compile in doc := baseDirectory.value / "docs/api"

// Logging
libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5", "org.slf4j" % "slf4j-simple" % "1.7.5")