
val sharedSettings = Seq(
  scalaVersion := "2.12.13"
)
val agent = project
  .settings(
    sharedSettings,
    packageOptions in (Compile, packageBin) += 
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

version := "0.3"

val bench = project
  .dependsOn(agent)
  .settings(
    sharedSettings,
    fork in run := true,

    libraryDependencies ++= Seq(
       "com.lihaoyi" %% "upickle" % "1.4.0",
       "com.lihaoyi" %% "pprint" % "0.6.6"
    )
    ,javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
    ,scalacOptions ++= Seq("-deprecation")
)
