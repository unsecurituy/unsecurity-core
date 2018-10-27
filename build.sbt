import sbt.Keys.libraryDependencies

organization := "io.unsecurity"

name := "unsecurity-core"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions := Seq("-deprecation", "-Ypartial-unification", "-language:higherKinds", "-Ywarn-value-discard")

val circeVersion      = "0.9.3"
val http4sVersion     = "0.18.11"
val directivesVersion = "0.5.1"

libraryDependencies := Seq(
  "io.circe"           %% "circe-parser"        % circeVersion,
  "org.http4s"         %% "http4s-blaze-client" % http4sVersion,
  "no.scalabin.http4s" %% "http4s-directives"   % directivesVersion,

  "org.scalatest"      %% "scalatest"           % "3.0.5" % Test
)
