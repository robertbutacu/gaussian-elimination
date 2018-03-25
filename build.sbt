name := "gaussian-elimination"

version := "0.1"

scalaVersion := "2.11.1"

val nd4jVersion = "0.9.1"
libraryDependencies += "org.nd4j" % "nd4j-native-platform" % nd4jVersion
libraryDependencies += "org.nd4j" %% "nd4s" % nd4jVersion