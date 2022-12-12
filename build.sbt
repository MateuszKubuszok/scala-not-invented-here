val common = Seq(
  organization := "not-invented-here",
  version := "0.1.0",
  scalaVersion := "3.2.1"
)

val io = project
  .in(file("io"))
  .settings(
    name := "nih-io"
  )
  .settings(common)

val ioCats = project
  .in(file("io-cats"))
  .settings(
    name := "nih-io-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.2",
      "com.lihaoyi" %% "sourcecode" % "0.3.0"
    )
  )
  .settings(common)
  .dependsOn(io)

val ioZio = project
  .in(file("io-zio"))
  .settings(
    name := "nih-io-zio",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.5"
    )
  )
  .settings(common)
  .dependsOn(io)

val root = project.in(file(".")).settings(common).aggregate(io, ioCats, ioZio)
