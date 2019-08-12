ThisBuild / scalaVersion := "2.12.8"

lazy val root = (project in file("."))
    .settings(
        name := "newsfeed",
        scalacOptions += "-Ypartial-unification",
        libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4",
        libraryDependencies += "io.getstream.client" % "stream-java" % "3.1.9",
        libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.0" withSources() withJavadoc(),
        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    )
