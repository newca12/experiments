scalaVersion in ThisBuild := "2.12.6"

lazy val commonSettings = Seq(
  organization := "org.edla",
  version := "0.2.0",
  //libraryDependencies += "org.scala-lang" % "scala-library" % "2.12.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  scalacOptions ++= Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-language:postfixOps", // Allow postfix operator notation
    "-unchecked",           // Enable additional warnings where generated code depends on assumptions.
    //Failed with Scala 2.12.3 "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    //"-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",  // Warn if a local definition is unused.
    //"-Ywarn-unused:params", // Warn if a value parameter is unused.
    //"-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
  )
)

lazy val interruptableComputations = project
  .in(file("interruptable-computation"))
  .settings(commonSettings: _*)
  .settings(scalacOptions -= "-Xfatal-warnings") //Thread.stop used
  .settings(
    libraryDependencies ++= {
      val akkaV = "2.5.12"
      Seq(
        "com.typesafe.akka" %% "akka-actor"      % akkaV,
        "com.twitter"       %% "util-collection" % "6.43.0" //last version with Future.cancel (check if raise could fit)
      )
    }
  )

lazy val javafx = project
  .in(file("javafx"))
  .settings(commonSettings: _*)
  //.settings(resolvers += Resolver.mavenLocal)
  .settings(
    libraryDependencies ++= {
      Seq(
        "org.controlsfx" % "controlsfx" % "8.40.14"
        //"nz.sodium.swidgets" % "sodium-swidgets" % "1.0.0"
      )
    }
  )

lazy val tika = project
  .in(file("tika"))
  .settings(commonSettings: _*)
  .settings(
    libraryDependencies ++= Seq(
      /* "org.apache.tika"    % "tika-bundle"       % "1.14",
      "org.apache.pdfbox"  % "pdfbox"            % "1.8.7",
      "com.uwyn"           % "jhighlight"        % "1.0",
      "org.apache.poi"     % "poi"               % "3.15",
      "org.apache.poi"     % "poi-ooxml"         % "3.15",
      "org.apache.poi"     % "poi-scratchpad"    % "3.15",
      "org.apache.commons" % "commons-compress"  % "1.13",
      "org.apache.poi"     % "poi-ooxml-schemas" % "3.15",
      "org.apache.poi"     % "ooxml-schemas"     % "1.3"*/
      "org.apache.tika"    % "tika-bundle"       % "1.18",
      "org.apache.pdfbox"  % "pdfbox"            % "2.0.9",
      "com.uwyn"           % "jhighlight"        % "1.0",
      "org.apache.poi"     % "poi"               % "3.17",
      "org.apache.poi"     % "poi-ooxml"         % "3.17",
      "org.apache.poi"     % "poi-scratchpad"    % "3.17",
      "org.apache.commons" % "commons-compress"  % "1.16.1",
      "org.apache.poi"     % "poi-ooxml-schemas" % "3.17",
      "org.apache.poi"     % "ooxml-schemas"     % "1.3"
    )
  )

lazy val experiments =
  (project in file("."))
    .aggregate(interruptableComputations, javafx, tika)
