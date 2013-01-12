import sbt._
import sbt.Keys._
import com.github.retronym._

object PencilBuild extends Build {

  lazy val pencil = Project(
    id = "pencil",
    base = file("."),
    settings = Project.defaultSettings ++ SbtOneJar.oneJarSettings ++ Seq(
      name := "pencil",
      organization := "com.aethereus",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
      libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.1.0"
    )
  )
}
