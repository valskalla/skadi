lazy val scalaVersions = List("2.13.2", "2.12.11")

lazy val `skadi-core` = project
  .in(file("core"))
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++=
      dependencies.catsEffect :: dependencies.cats :: Nil
  )

lazy val `skadi-laws` = project
  .in(file("laws"))
  .dependsOn(`skadi-core` % "compile->compile")
  .settings(sharedSettings)
  .settings(libraryDependencies ++= dependencies.catsLaws :: Nil)

lazy val `skadi-monix` = project
  .in(file("monix"))
  .dependsOn(`skadi-core`, `skadi-laws`, tests % "test->test")
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++= dependencies.monix :: Nil
  )

lazy val `skadi-opentracing` = project
  .in(file("opentracing"))
  .dependsOn(`skadi-core`, tests % "test->test")
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++= dependencies.openTracing :: dependencies.opentracingMock :: Nil
  )

lazy val tests = project
  .in(file("tests"))
  .dependsOn(`skadi-core`, `skadi-laws`)
  .settings(sharedSettings)
  .settings(noPublish)
  .settings(
    libraryDependencies ++=
      dependencies.scalaTest ::
        dependencies.scalaTestCheck ::
        dependencies.scalaCheck :: Nil
  )

lazy val docs = (project in file("skadi-docs"))
  .settings(sharedSettings)
  .settings(noPublish)
  .settings(
    mdocVariables := Map(
      "VERSION" -> version.value
    ),
    mdocOut := file(".")
  )
  .dependsOn(`skadi-core`, `skadi-laws`, `skadi-monix`, `skadi-opentracing`)
  .enablePlugins(MdocPlugin)

lazy val root = project
  .in(file("."))
  .settings(
    sharedSettings
  )
  .aggregate(
    `skadi-core`,
    `skadi-laws`,
    `skadi-opentracing`,
    `skadi-monix`,
    tests
  )

lazy val dependencies = new {
  lazy val cats = "org.typelevel" %% "cats-core" % "2.3.0"

  lazy val catsLaws = "org.typelevel" %% "cats-laws" % "2.3.0"

  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "2.3.0"

  lazy val monix = "io.monix" %% "monix" % "3.3.0"

  lazy val openTracing = "io.opentracing" % "opentracing-api" % "0.33.0"

  lazy val scalaTestCheck = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" % Test

  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.1" % Test

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.1.3" % Test

  lazy val opentracingMock = "io.opentracing" % "opentracing-mock" % "0.33.0" % Test
}

lazy val sharedSettings = Seq(
  scalaVersion := "2.13.2",
  organization := "com.github.valskalla",
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  //crossScalaVersions := scalaVersions,
  classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary,
  scalacOptions := scalacOptionsVersion(scalaVersion.value),
  scalacOptions in (Compile, console) ~= (_.filterNot(
    Set(
      "-Ywarn-unused:imports",
      "-Xfatal-warnings",
      "-Wunused:implicits",
      "-Werror"
    )
  )),
  homepage := Some(url("https://github.com/valskalla/skadi")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "sergeykolbasov",
      "Sergey Kolbasov",
      "whoisliar@gmail.com",
      url("https://github.com/sergeykolbasov")
    )
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val noPublish = Seq(
  skip in publish := true
)

def scalacOptionsVersion(scalaVersion: String) =
  Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-language:postfixOps", // Allow postfix operators
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
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
    "-Xlint:type-parameter-shadow" // A local type parameter shadows a type already in scope.
  ) ++ (CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, scalaMajor)) if scalaMajor == 12 => scalac212Options
    case Some((2, scalaMajor)) if scalaMajor == 13 => scalac213Options
  })

lazy val scalac212Options = Seq(
  "-Xfuture", // Turn on future language features.
  "-Xfatal-warnings", // Fail the compilation if there are any warnings.
  "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification", // Enable partial unification in type constructor inference
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:unsound-match", // Pattern match may not be typesafe.
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-dead-code", // Warn when dead code is identified.
  "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
  "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals", // Warn if a local definition is unused.
  "-Ywarn-unused:params", // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates" // Warn if a private member is unused.
)

lazy val scalac213Options = Seq(
  "-Werror",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wunused:implicits",
  "-Wunused:imports",
  "-Wunused:patvars",
  "-Wunused:privates"
  //"-Wunused:params"
)
