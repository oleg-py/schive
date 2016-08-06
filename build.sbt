name := "schive"
version := "0.1.0"
scalaVersion := "2.11.8"

libraryDependencies += "net.sf.sevenzipjbinding" % "sevenzipjbinding" % "9.20-2.00beta"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "net.sf.sevenzipjbinding" % "sevenzipjbinding-all-platforms" % "9.20-2.00beta" % "test"

scalacOptions ++= Seq("feature", "deprecation")
