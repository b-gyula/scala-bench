import mill._, scalalib._, scalalib.publish._

val scalaVersions = Seq("2.12.13", "2.13.14", "3.3.3")

trait MyModule extends PublishModule
	with ScalaModule {

	def scalaVersion = "2.12.13"
	override
	def scalacOptions = Seq("-deprecation")
	def publishVersion = "0.3.2"
	def pomSettings = PomSettings(
		description = "",
		organization = "scala.coll",
		url = "",
		licenses = Nil,
		VersionControl(),
		developers = Nil
	)
	// Do not generate docJar
	override def docSources = T{Seq[PathRef]()}
}

trait BenchModule extends MyModule {
	override
	def ivyDeps =  Agg(
		ivy"com.lihaoyi::upickle:3.3.0"
		,ivy"com.lihaoyi::mainargs:0.7.0"
		,ivy"com.lihaoyi::pprint:0.9.0")
	override
	def moduleDeps = Seq(agent)
	override
	def mainClass = Some("bench.Performance")
}

object bench extends BenchModule


object agent extends MyModule