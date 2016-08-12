name := "schive"
version := "0.1.0"
scalaVersion := "2.11.8"

libraryDependencies += "net.sf.sevenzipjbinding" % "sevenzipjbinding" % "9.20-2.00beta"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "net.sf.sevenzipjbinding" % "sevenzipjbinding-all-platforms" % "9.20-2.00beta" % "test"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "2.16.0" % "test"

scalacOptions ++= Seq(
//  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-unchecked",
  "-feature",
  "-deprecation",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Ywarn-unused",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xfuture",
  "-Ywarn-unused-import"
)
