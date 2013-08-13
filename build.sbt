name := "bigint"

scalaVersion := "2.11.0-M4"

compileOrder := CompileOrder.JavaThenScala

scalacOptions ++= Seq("-optimize","-Yinline-warnings")

resolvers ++= Seq(
  "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)