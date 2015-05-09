name := "network-protocol-dsl"

version := "0.1"

scalaVersion := "2.11.5"

scalacOptions ++= Seq("-deprecation", "-feature")

resolvers += Resolver.mavenLocal

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.9",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "org.jasypt" % "jasypt" % "1.9.2",
  "net.liftweb" %% "lift-json" % "2.6"
)
