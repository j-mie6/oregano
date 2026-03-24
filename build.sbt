val projectName = "oregano"
val Scala3 = "3.7.4"

val Java11 = JavaSpec.temurin("11")
val Java17 = JavaSpec.temurin("17")
val Java21 = JavaSpec.temurin("21")

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(List(
    tlBaseVersion := "0.1",
    organization := "com.github.j-mie6",
    organizationName := "Oregano Contributors <https://github.com/j-mie6/oregano/graphs/contributors>",
    startYear := Some(2024),
    homepage := Some(url("https://github.com/j-mie6/oregano")),
    licenses := List("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")),
    versionScheme := Some("early-semver"),
    crossScalaVersions := Seq(Scala3),
    scalaVersion := Scala3,
    // CI Configuration
    tlCiReleaseBranches := Seq("main"),
    tlCiScalafmtCheck := false,
    tlCiHeaderCheck := true,
    githubWorkflowJavaVersions := Seq(Java11, Java17, Java21),
))

lazy val root = tlCrossRootProject.aggregate(oregano)

lazy val oregano = crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("oregano"))
    .settings(
        name := projectName,
        headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
        headerEmptyLine := false,

        libraryDependencies ++= Seq(
            "com.github.j-mie6" %%% "parsley" % "5.0.0-M19",
            //"com.github.j-mie6" %%% "parsley-debug" % "5.0.0-M19",
            "org.typelevel" %%% "cats-collections-core" % "0.9.10",
            "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
            "org.scalatestplus" %%% "scalacheck-1-18" % "3.2.19.0" % Test,
        ),

        Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oI"),
        scalacOptions += "-Yexplicit-nulls",
    )
    .jsSettings(
        libraryDependencies += "org.scala-lang" %% "scala3-library" % scalaVersion.value
    )

lazy val benchmark = project
  .in(file("benchmark"))
  .enablePlugins(JmhPlugin)
  .dependsOn(oregano.jvm)
  .settings(
    name := "oregano-benchmark",
    scalaVersion := Scala3,
    fork := true,
    libraryDependencies ++= Seq(
      "org.openjdk.jmh" % "jmh-core" % "1.37",
      "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.37"
    )
  )
