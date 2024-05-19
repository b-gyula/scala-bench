val sharedSettings = Seq(
  scalaVersion := "2.12.13"
  ,version := "0.4"
)

val agent = project
  .settings(
    sharedSettings,
    Compile / packageBin / packageOptions +=
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(
    sharedSettings,
     run / fork := true,

    libraryDependencies ++= Seq(
       "com.lihaoyi" %% "upickle" % "1.4.0",
       "com.lihaoyi" %% "pprint" % "0.6.6",
       "com.lihaoyi" %% "mainargs" % "0.2.1"
    )
    ,run / javaOptions += ("-javaagent:" + (packageBin in (agent, Compile)).value)
    ,scalacOptions ++= Seq("-deprecation")
)
