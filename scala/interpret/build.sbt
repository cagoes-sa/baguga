ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

val enumeratumVersion = "1.7.5"

libraryDependencies ++= Seq(
  "com.beachape" %% "enumeratum" % enumeratumVersion
)

lazy val root = (project in file("."))
  .settings(
    name := "interpret"
  )
