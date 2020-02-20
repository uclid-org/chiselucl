// Basic settings
organization := "edu.berkeley.cs"
name := "chiselucl"
version := "0.2-SNAPSHOT"
scalaVersion := "2.12.7"
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
//crossScalaVersions := Nil

// Use Scala 2.11 source compatibility flag to allow anonymous bundle idiom
scalacOptions := Seq("-Xsource:2.11", "-Ywarn-unused-import")

// Enable Sonatype snapshots, if needed
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Managed dependency on latest FIRRTL + chisel3
//libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.2.0-RC1"
libraryDependencies += "edu.berkeley.cs" %% "firrtl" % "1.2.2"
//libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2.0-RC1"
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2.2"

libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.1"

