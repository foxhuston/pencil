import sbt._
import sbt.Keys._

object PencilBuild extends Build {

  lazy val pencil = Project(
    id = "pencil",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "pencil",
      organization := "com.aethereus",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1"
    )
  )
}
