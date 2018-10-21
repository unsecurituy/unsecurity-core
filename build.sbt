organization := "io.unsecurity"

name := "unsecurity-core"

version := "0.1"

scalaVersion := "2.12.7"

scalacOptions := Seq("-deprecation", "-Ypartial-unification", "-language:higherKinds", "-Ywarn-value-discard")

val circeVersion           = "0.9.3"
val http4sVersion          = "0.18.11"

libraryDependencies := Seq(
  "io.circe"                 %% "circe-parser"        % circeVersion,
  "org.http4s"               %% "http4s-blaze-client" % http4sVersion,
)
