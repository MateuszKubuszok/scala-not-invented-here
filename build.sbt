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
