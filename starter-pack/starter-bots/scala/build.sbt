name := "Entelect-Scala-Bot"

version := "0.1"

scalaVersion := "2.12.2"

val json4sNative = "org.json4s" %% "json4s-native" % "3.5.4"

libraryDependencies ++= {
  Seq(
    json4sNative
  )
}