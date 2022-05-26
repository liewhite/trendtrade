val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "trendTrade",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.6.2",
    libraryDependencies += "com.softwaremill.sttp.client3" %% "okhttp-backend" % "3.6.2",
    libraryDependencies += "io.github.liewhite" %% "json" % "0.17.3",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
  )
