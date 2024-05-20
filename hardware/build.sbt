ThisBuild / version := "0.1.0-SNAPSHOT"
Test / parallelExecution := false
ThisBuild / scalaVersion := "2.12.12"

lazy val root = (project in file("."))
  .settings(
    name := "ScalaLearn3"
  )
addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % "3.5.4" cross CrossVersion.full)
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.5.4"
// We also recommend using chiseltest for writing unit tests
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5.4" % "test"

