name := "FunctionalMonopoly"

version := "0.1"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.8"

libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.11" % Test
