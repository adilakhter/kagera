import sbt._

object Dependencies {

  val akkaVersion = "2.4.17"
  val akkaHttpVersion = "2.4.11"
  val cytoscapeVersion = "2.7.9"

  val akkaActor                = "com.typesafe.akka"               %% "akka-actor"                          % akkaVersion
  val akkaPersistence          = "com.typesafe.akka"               %% "akka-persistence"                    % akkaVersion
  val akkaTestkit              = "com.typesafe.akka"               %% "akka-testkit"                        % akkaVersion
  val akkaSlf4j                = "com.typesafe.akka"               %% "akka-slf4j"                          % akkaVersion
  val akkaStream               = "com.typesafe.akka"               %% "akka-stream"                         % akkaVersion
  val akkaStreamTestKit        = "com.typesafe.akka"               %% "akka-stream-testkit"                 % akkaVersion
  val akkaQuery                = "com.typesafe.akka"               %% "akka-persistence-query-experimental" % akkaVersion
  val akkaClusterSharding      = "com.typesafe.akka"               %% "akka-cluster-sharding"               % akkaVersion
  val akkaHttp                 = "com.typesafe.akka"               %% "akka-http-experimental"              % akkaHttpVersion
  val akkaInmemoryJournal      = "com.github.dnvriend"             %% "akka-persistence-inmemory"           % "2.4.17.1"

  val akkaPersistenceCassandra = "com.typesafe.akka"               %% "akka-persistence-cassandra" % "0.23"
  val akkaPersistenceQuery     = "com.typesafe.akka"               %% "akka-persistence-query-experimental" % akkaVersion

  val scalaGraph               = "org.scala-graph"                 %% "graph-core"             % "1.11.4"
  val scalaGraphDot            = "org.scala-graph"                 %% "graph-dot"              % "1.11.0"

  val fs2Core                  = "co.fs2"                          %% "fs2-core"               % "0.9.4"
  val catsCore                 = "org.typelevel"                   %% "cats-core"              % "0.9.0"

  val logback                  = "ch.qos.logback"                  %  "logback-classic"        % "1.2.2"
  val ficus                    = "net.ceedubs"                     %% "ficus"                  % "1.1.2"
  val scalaReflect             = "org.scala-lang"                  % "scala-reflect"           % "2.11.8"
  val scalatest                = "org.scalatest"                   %% "scalatest"              % "2.2.1"
  val scalaCheck               = "org.scalacheck"                  %% "scalacheck"             % "1.13.4"
  val mockito                  = "org.mockito"                     %  "mockito-all"            % "1.10.19"
  val shapeless                = "com.chuusai"                     %% "shapeless"              % "2.3.2"
}




