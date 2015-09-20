import sbt.complete.DefaultParsers._

val pickle = InputKey[Unit]("pickle")
val evolve = InputKey[Unit]("evolve")
val replay = InputKey[Unit]("replay")

fork in run := true

// give log4j configuration file to logger
javaOptions in run += "-Dlog4j.configurationFile=conf/logger_config.xml"

pickle := {
	(runMain in Compile).partialInput(s" edu.uga.pickle.MainApplication ").evaluated
}

evolve := {
	(runMain in Compile).partialInput(s" edu.uga.pickle.evolve.Evolver init ").evaluated
}

replay := {
	(runMain in Compile).partialInput(s" edu.uga.pickle.evolve.Evolver replay ").evaluated
}


lazy val root = ( project in file(".") ).
	settings(

		name := "Pickle",

		version := "0.1.0",

		scalaVersion := "2.11.7",

		scalaSource := file("src"),

		libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.2",

		libraryDependencies += "commons-io" % "commons-io" % "2.4",

		libraryDependencies += "jfree" % "jcommon" % "1.0.16",

		libraryDependencies += "javax.media" % "jmf" % "2.1.1e",

		libraryDependencies += "org.jfree" % "jfreechart" % "1.0.19",

		libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.1",

		libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.1"

)
