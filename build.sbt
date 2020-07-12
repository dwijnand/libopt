val libopt = project in file(".")

ThisBuild / organization   := "com.dwijnand"
ThisBuild /      version   := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion   := "2.13.3"
ThisBuild / scalacOptions ++= List("-deprecation")

libraryDependencies += "org.scalacheck" %% "scalacheck"       % "1.14.3" % Test
libraryDependencies += "org.scalameta"  %% "munit"            % "0.7.9"  % Test
libraryDependencies += "org.scalameta"  %% "munit-scalacheck" % "0.7.9" % Test
testFrameworks += new TestFramework("munit.Framework")
