
val sharedSettings = Seq(
  scalaVersion := "2.12.13"
)
val agent = project
  .settings(
    sharedSettings,
    packageOptions in (Compile, packageBin) += 
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(
    sharedSettings,
    fork in run := true,

    libraryDependencies ++= Seq(
       "com.lihaoyi" %% "ammonite-ops" % "2.4.0",
       "com.lihaoyi" %% "upickle" % "1.4.0",
       "com.lihaoyi" %% "pprint" % "0.6.6"
    ),
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
)
