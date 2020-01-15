scalaVersion := "2.13.1"

name := "scalaml"
version := "1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

logBuffered in Test := false
