val scala3Version = "3.2.2"

ThisBuild / assemblyMergeStrategy  := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("/module-info.class") => MergeStrategy.discard
  case x if x.contains(".class") => MergeStrategy.first
  case x => MergeStrategy
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}
val zioVersion = "2.0.10"
val zioJsonVersion = "0.5.0"
val zioConfigVersion = "4.0.0-RC14"

val zioDeps = Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-streams" % zioVersion,
  "dev.zio" %% "zio-json" % zioJsonVersion,
  "dev.zio" %% "zio-config" % zioConfigVersion,
  "dev.zio" %% "zio-config-magnolia" % zioConfigVersion,
  "dev.zio" %% "zio-config-yaml" % zioConfigVersion,
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "trendTrade",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.softwaremill.sttp.client3" %% "core" % "3.6.2",
    libraryDependencies += "com.softwaremill.sttp.client3" %% "okhttp-backend" % "3.6.2",
    libraryDependencies += "commons-codec" % "commons-codec" % "1.15",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies ++= zioDeps,
    assembly / mainClass := Some("main"),
  )
