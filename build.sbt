name := "math-logic"
version := "0.1"
scalaVersion := "2.12.5"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test    := baseDirectory.value / "test"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"
