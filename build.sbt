name := (name in ThisBuild).value

inThisBuild(Seq(
  name := "LogicToProm",
  organization := "com.dafttech",
  version := "0.0.1",

  scalaVersion := "2.13.5"
))

lazy val root = project.in(file("."))
  .settings(publishArtifact := false)
  .aggregate(
    logictoprom
  )

lazy val logictoprom = project
  .settings(name := (name in ThisBuild).value)
