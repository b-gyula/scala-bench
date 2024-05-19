import mill._, scalalib._, scalalib.publish._
import mill.util.Jvm
import mill.api.{PathRef, Result}

val scalaVersions = Seq("2.12.13", "2.13.14", "3.3.1")

trait MyModule extends PublishModule
	with ScalaModule {
	//override def ammoniteVersion = "2.5.11"
	def scalaVersion = scalaVersions(0)
	override
	def scalacOptions = Seq("-deprecation")
	def publishVersion = "0.4"
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
	def ivyDeps = Agg(
		 ivy"com.lihaoyi::upickle:3.3.0"
		,ivy"com.lihaoyi::mainargs:0.7.0"
		,ivy"com.lihaoyi::pprint:0.9.0")
	override
	def moduleDeps = Seq(agent)
	override
	def mainClass = Some("bench.Performance")
}

object bench extends BenchModule {
	def Memory(args: Task[Args] = T.task(Args())) = T.command {
		try Result.Success(
			Jvm.runSubprocess(
				"bench.Memory",
				runClasspath().map(_.path),
				forkArgs() :+ "-javaagent:" + agent.jar().path,
				forkEnv(),
				args().value,
				workingDir = forkWorkingDir(),
				useCpPassingJar = runUseArgsFile()
			)
		)
		catch {
			case e: Exception =>
				Result.Failure("subprocess failed:" + e.getMessage)
		}
	}
}

object agent extends MyModule {
	override def manifest = T {
		super.manifest().add("Premain-Class" -> "agent.Agent")
	}
}