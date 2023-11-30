lazy val extractSegments = (project in file("extract-segments"))

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.scala-lang.modules" %% "scala-xml" % "2.0.0")