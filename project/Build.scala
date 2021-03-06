import sbt._
import Keys._

object Build extends Build {

  lazy val project = Project("root", file(".")).settings(

    // basics
    name := "hashset",
    organization := "rklaehn",
    version := "1.0.0-SNAPSHOT",
    scalaVersion := "2.10.3",
    scalacOptions += "-optimize",

    // dependencies
    libraryDependencies ++= Seq(
        "com.novocode" % "junit-interface" % "0.10" % "test",
//        "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
//        "com.google.code.gson" % "gson" % "1.7.1",
//        "com.google.caliper" % "caliper" % "0.5-rc1",
        "junit" % "junit" % "4.10",
        "org.scalatest" %% "scalatest" % "1.9.1"
    ),
    resolvers += "sonatypeSnapshots" at "http://oss.sonatype.org/content/repositories/snapshots",

    // enable forking in run
    fork in run := true,

    // we need to add the runtime classpath as a "-cp" argument to the `javaOptions in run`, otherwise caliper
    // will not see the right classpath and die with a ConfigurationException
    javaOptions in run <++= (fullClasspath in Runtime) map { cp => Seq("-cp", sbt.Build.data(cp).mkString(":")) }
  )
}
