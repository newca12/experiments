scalafmtOnCompile in ThisBuild := true
scalafmtVersion in ThisBuild := "1.1.0"
scalaVersion in ThisBuild := "2.12.2"

lazy val commonSettings = Seq(
  organization := "org.edla",
  version := "0.2.0",
  //libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % "test",
  // Adapted from Rob Norris' post at https://tpolecat.github.io/2014/04/11/scalac-flags.html
  scalacOptions ++= Seq(
    "-language:postfixOps",
    "-language:existentials",
    "-language:implicitConversions",
    //"-optimize",
    "-deprecation",
    "-encoding", // yes this
    "UTF-8", // is 2 args
    "-feature",
    "-unchecked",
    //"-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture",
    "-Ywarn-unused-import"
  )
)

lazy val interruptableComputations = project
  .in(file("interruptable-computation"))
  .settings(commonSettings: _*)
  .settings(scalacOptions -= "-Xfatal-warnings") //Thread.stop used
  .settings(
    libraryDependencies ++= {
      val akkaV = "2.5.3"
      Seq(
        "com.typesafe.akka" %% "akka-actor"      % akkaV,
        "com.twitter"       %% "util-collection" % "6.43.0" //last version with Future.cancel (check if raise could fit)
      )
    }
  )

lazy val experiments =
  (project in file("."))
    .aggregate(interruptableComputations)
