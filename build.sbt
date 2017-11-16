import com.typesafe.sbt.pgp.PgpKeys.publishSigned

lazy val core = crossProject
  .in(file("core"))
  .settings(buildSettings: _*)
  .settings(publishSettings: _*)
  .settings(scalaMacroDependencies: _*)
  .settings(moduleName := "optometry")

lazy val coreJVM = core.jvm
lazy val coreJS = core.js

lazy val tests = project
  .in(file("tests"))
  .settings(buildSettings: _*)
  .settings(unmanagedSettings)
  .settings(moduleName := "optometry-tests")
  .dependsOn(coreJVM)

lazy val buildSettings = Seq(
  organization := "com.propensive",
  scalaVersion := "2.12.4",
  name := "optometry",
  version := "0.1.0",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-Ywarn-value-discard",
    "-Ywarn-dead-code",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-inaccessible",
    "-Ywarn-adapted-args"
  ),
  scmInfo := Some(
    ScmInfo(url("https://github.com/propensive/optometry"),
            "scm:git:git@github.com:propensive/optometry.git")
  )
)

lazy val publishSettings = Seq(
  homepage := Some(url("http://optometry.propensive.com/")),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ =>
    false
  },
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra := (
    <developers>
      <developer>
        <id>propensive</id>
        <name>Jon Pretty</name>
        <url>https://github.com/propensive/optometry/</url>
      </developer>
    </developers>
  )
)

import java.io.File

lazy val unmanagedSettings = unmanagedBase := (scalaVersion.value
  .split("\\.")
  .map(_.toInt)
  .to[List] match {
  case List(2, 12, _) => baseDirectory.value / "lib" / "2.12"
  case List(2, 11, _) => baseDirectory.value / "lib" / "2.11"
})

lazy val scalaMacroDependencies: Seq[Setting[_]] = Seq(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
)
