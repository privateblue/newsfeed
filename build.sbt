ThisBuild / scalaVersion := "2.12.8"

lazy val root = (project in file("."))
    .settings(
        name := "newsfeed",
        scalacOptions += "-Ypartial-unification",
        resolvers += Resolver.sonatypeRepo("releases"),
        addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
        libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4",
        libraryDependencies += "org.typelevel" %% "cats-effect" % "1.3.0" withSources() withJavadoc(),
        libraryDependencies += "io.getstream.client" % "stream-java" % "3.1.9",
        libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.2",
    )
