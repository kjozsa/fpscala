name := "fpscala"

version := "0.0.1"

organization := "com.fsdev"

scalaVersion := "2.11.4"

resolvers ++= Seq(
		"snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
		"releases" at "http://oss.sonatype.org/content/repositories/releases"
		)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= {
	Seq(
			"org.scalatest" % "scalatest_2.11" % "2.2.2"
	   )
}


