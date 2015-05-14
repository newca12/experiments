import scalariform.formatter.preferences._

lazy val commonSettings = Seq(
  organization := "org.edla",
  version := "0.1.0",
  scalaVersion := "2.11.6",
  scalacOptions ++= Seq(
    "-language:postfixOps", "-language:existentials", "-language:implicitConversions",
    //"-optimize",
    "-deprecation",
    "-encoding", "UTF-8", // yes, this is 2 args
    "-feature",
    "-unchecked",
    //"-Xfatal-warnings",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-value-discard",
    "-Xfuture"
  )
) ++ scalariformSettings :+
  (ScalariformKeys.preferences := FormattingPreferences().
    setPreference(RewriteArrowSymbols, true).
    setPreference(AlignParameters, true).
    setPreference(AlignSingleLineCaseStatements, true).
    setPreference(DoubleIndentClassDeclaration, true))

lazy val interruptableComputations = (project in file("interruptable-computations")).
  settings(commonSettings: _*).
  settings( // other settings
  )

lazy val privateObject = (project in file("private-object")).
  settings(commonSettings: _*).
  settings( // other settings
  )
