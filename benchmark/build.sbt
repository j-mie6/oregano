enablePlugins(JmhPlugin)

ThisBuild / scalaVersion := "3.5.0"

fork := true

libraryDependencies ++= Seq(
  "org.openjdk.jmh" % "jmh-core" % "1.37",
  "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.37"
)
