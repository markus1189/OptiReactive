name := "OptiReactive"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0"

libraryDependencies += "EPFL" %% "lms" % "0.3-SNAPSHOT"

scalacOptions += "-Yvirtualize"

libraryDependencies ++= Seq( "org.scalatest" %% "scalatest" % "1.9.1" % "test"
                           , "org.mockito" % "mockito-all" % "1.9.5"
                           , "junit" % "junit" % "4.10" % "test->default"
                           , "com.novocode" % "junit-interface" % "0.8" % "test->default"
                           )