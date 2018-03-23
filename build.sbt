name := "math-logic"
version := "0.1"
scalaVersion := "2.12.5"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test    := baseDirectory.value / "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
