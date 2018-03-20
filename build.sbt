name := "gaussian-elimination"

version := "0.1"

scalaVersion := "2.12.5"

val nd4jVersion = "0.7.2"

libraryDependencies += "org.nd4j" % "nd4j-native-platform" % nd4jVersion
libraryDependencies += "org.nd4j" %% "nd4s" % nd4jVersion