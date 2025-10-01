val projectName = "oregano"
val Scala3 = "3.7.1"

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
    tlCiReleaseBranches := Seq.empty, //Seq("main"),
    tlCiScalafmtCheck := false,
    tlCiHeaderCheck := true,
    githubWorkflowJavaVersions := Seq(JavaSpec.temurin("8"), JavaSpec.temurin("11"), JavaSpec.temurin("17")),
))

lazy val root = tlCrossRootProject.aggregate(oregano, benchmark)

lazy val oregano = crossProject(JVMPlatform, JSPlatform, NativePlatform)
    .withoutSuffixFor(JVMPlatform)
    .crossType(CrossType.Full)
    .in(file("oregano"))
    .settings(
        name := projectName,
        headerLicenseStyle := HeaderLicenseStyle.SpdxSyntax,
        headerEmptyLine := false,

        // scalaJSUseMainModuleInitializer := true,
        // Compile / mainClass := Some("oregano.shared.ManualBench"),

        resolvers ++= Opts.resolver.sonatypeOssReleases, // Will speed up MiMA during fast back-to-back releases
        resolvers ++= Opts.resolver.sonatypeOssSnapshots,
        libraryDependencies ++= Seq(
            "com.github.j-mie6" %%% "parsley" % "5.0-bdb596b-SNAPSHOT",
            "com.github.j-mie6" %%% "parsley-debug" % "5.0-bdb596b-SNAPSHOT",
            "org.typelevel" %%% "cats-collections-core" % "0.9.8",         // NOTE: held back for 0.4 native
            "org.scalatest" %%% "scalatest" % "3.2.18" % Test,             // NOTE: held back for 0.4 native
            "org.scalacheck" %%% "scalacheck" % "1.17.1" % Test,           // NOTE: held back for 0.4 native
            "org.scalatestplus" %%% "scalacheck-1-17" % "3.2.18.0" % Test, // NOTE: held back for 0.4 native
        ),
        
        Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oI"),
    )

lazy val oreganoJvm = oregano.jvm.settings(
  libraryDependencies += "com.google.re2j" % "re2j" % "1.8",
  libraryDependencies += "codes.quine.labo" %% "re2s" % "0.1.1-SNAPSHOT",
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
