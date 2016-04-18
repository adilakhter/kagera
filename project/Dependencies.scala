import sbt.Keys._
import sbt._

object Dependencies {

  val akkaVersion = "2.4.2"
  val sprayVersion = "1.3.2"

  val akkaActor             = "com.typesafe.akka"               %% "akka-actor"             % akkaVersion
  val akkaPersistence       = "com.typesafe.akka"               %% "akka-persistence"       % akkaVersion
  val akkaTestkit           = "com.typesafe.akka"               %% "akka-testkit"           % akkaVersion
  val akkaSlf4j             = "com.typesafe.akka"               %% "akka-slf4j"             % akkaVersion
  val akkaHttp              = "com.typesafe.akka"               %% "akka-http-experimental" % akkaVersion

  val graph                 = "com.assembla.scala-incubator"    %% "graph-core"             % "1.10.1"
  val graphDot              = "com.assembla.scala-incubator"    %% "graph-dot"              % "1.10.1"

  val logback               = "ch.qos.logback"                  %  "logback-classic"        % "1.1.2"
  val ficus                 = "net.ceedubs"                     %% "ficus"                  % "1.1.2"
  val scalatest             = "org.scalatest"                   %% "scalatest"              % "2.2.1"
  val scalaTime             = "com.github.nscala-time"          %% "nscala-time"            % "1.6.0"
  val scalaz                = "org.scalaz"                      %% "scalaz-core"            % "7.1.3"
}
