val nm = "sgxlib"
val ver = "1.0.4"

// Basic info
name := nm
version := ver
scalaVersion := "2.12.0"

// sbt assembly to generate fat .jar
excludeFilter in unmanagedSources := HiddenFileFilter || "*scoverage*"
assemblyJarName in assembly := nm + "-" + ver + ".jar"

// Compiler options
scalacOptions += "-feature"

// Scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
testOptions in Test += Tests.Argument("-oF")

// Coveralls
import org.scoverage.coveralls.Imports.CoverallsKeys._
coverallsTokenFile := Some("src/main/resources/token.txt")
coverageEnabled in(Test, compile) := true
coverageEnabled in(Compile, compile) := false

// htsjdk
unmanagedJars in Compile += file("lib/htsjdk-2.9.0-1-g55bf01b-SNAPSHOT.jar")
apiMappings += (
  (unmanagedBase.value / "lib/htsjdk-2.9.0-1-g55bf01b-SNAPSHOT.jar") ->
    url("http://samtools.github.io/htsjdk/javadoc/htsjdk/")
  )

// Scaladoc
target in Compile in doc := baseDirectory.value / "docs/api"

// Logging
libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5", "org.slf4j" % "slf4j-simple" % "1.7.5")
