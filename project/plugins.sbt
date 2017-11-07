logLevel := Level.Warn

resolvers ++= Seq(
  "artifactory-maven" at "http://lolhens.no-ip.org/artifactory/maven-public/",
  Resolver.url("artifactory-ivy", url("http://lolhens.no-ip.org/artifactory/ivy-public/"))(Resolver.ivyStylePatterns)
)

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.3.3")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.2.3")

//addSbtPlugin("com.lucidchart" % "sbt-cross" % "3.0")

//addSbtPlugin("org.scala-native" % "sbt-crossproject" % "0.2.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.21")

//addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.3.2")
