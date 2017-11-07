name := (name in ThisBuild).value

inThisBuild(Seq(
  name := "LogicToProm",
  organization := "com.dafttech",
  version := "0.0.1",

  scalaVersion := "2.12.4",

  resolvers ++= Seq(
    "artifactory-maven" at "http://lolhens.no-ip.org/artifactory/maven-public/",
    Resolver.url("artifactory-ivy", url("http://lolhens.no-ip.org/artifactory/ivy-public/"))(Resolver.ivyStylePatterns)
  ),

  scalacOptions ++= Seq("-Xmax-classfile-name", "127")
))

lazy val root = project.in(file("."))
  .settings(publishArtifact := false)
  .aggregate(
    logictopromJVM_2_12,
    logictopromJS_2_12
  )

lazy val logictoprom = crossProject.crossType(CrossType.Pure)
  .settings(name := (name in ThisBuild).value)
  .settings(
    libraryDependencies ++= Seq(
      //"org.typelevel" %%% "cats" % "0.9.0"
    )
  )

lazy val logictopromJVM_2_12 = logictoprom.jvm.settings(name := (name in ThisBuild).value)
lazy val logictopromJS_2_12 = logictoprom.js.settings(name := (name in ThisBuild).value)
