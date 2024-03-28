ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.13"

// available for 2.12, 2.13, 3.2
libraryDependencies += "co.fs2" %% "fs2-core" % "3.9.4"

// optional I/O library
libraryDependencies += "co.fs2" %% "fs2-io" % "3.9.4"

// optional reactive streams interop
libraryDependencies += "co.fs2" %% "fs2-reactive-streams" % "3.9.4"

// optional scodec interop
libraryDependencies += "co.fs2" %% "fs2-scodec" % "3.9.4"

lazy val root = (project in file("."))
  .settings(
    name := "fs2 - udemy course"
  )
