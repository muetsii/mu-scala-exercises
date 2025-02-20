lazy val example = project.in(file("."))
  .settings(
    scalaVersion := "3.3.4",
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7"
  )
