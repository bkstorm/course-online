name := course.value + "-" + assignment.value

scalaVersion := "2.11.12"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1"
libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "4.1.0" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

// include the common dir
commonSourcePackages += "common"

courseId := "PeZYFz-zEeWB_AoW1KYI4Q"
