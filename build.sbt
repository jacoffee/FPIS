name := "fpis"

organization := "jacoffee"

version := "1.0"

scalaVersion := "2.10.5"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-language:higherKinds",
  "-language:postfixOps"
)

resolvers ++= Seq(
  "osc" at "http://maven.oschina.net/content/groups/public/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "typesafe" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.3"
)

//initialCommands in console := "import ch06.RNG._"