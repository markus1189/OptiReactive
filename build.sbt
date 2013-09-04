name := "OptiReactive"

scalacOptions ++= Seq( "-deprecation"
                     , "-unchecked"
                     , "-feature"
                     , "-optimise"
                     , "-Yinline-warnings"
                     , "-Yvirtualize")

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq( "org.scalatest" %% "scalatest" % "2.0.M6"
                           , "EPFL" %% "lms" % "0.3-SNAPSHOT"
                           )