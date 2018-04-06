name := "Strategy"

version := "0.1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.specs2" %% "specs2-core" % "4.0.2" % "test"
libraryDependencies += "com.rojoma" %% "rojoma-json-v3" % "3.8.0"
libraryDependencies += "joda-time" % "joda-time" % "2.9.9"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0"

scalacOptions in Test ++= Seq("-Yrangepos")

