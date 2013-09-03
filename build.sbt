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
                           , "org.mockito" % "mockito-all" % "1.9.5"
                           , "junit" % "junit" % "4.10" % "test->default"
                           , "com.novocode" % "junit-interface" % "0.8" % "test->default"
                           )