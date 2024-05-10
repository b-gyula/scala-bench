package bench

import bench.Benchmark.{Case, typesFromString}
import mainargs.{ParserForMethods, arg, main}

import scala.collection.{mutable => m}
import java.io.{File, FileOutputStream}
import java.text.NumberFormat
import java.util.Locale

object Performance{
	val defaultResultFileName = s"result.$scalaVersion.json"

	def calcWidth(i: Int, size: Int): Int = {
		val maxWidth = 15
		maxWidth - ((size - i).toDouble / size * maxWidth / 2).round.toInt
	}

	def printRow[I: Integral](name: String, items: Seq[I]): Unit = {
		println(
			name.padTo(19, ' ') +
				items.map(NumberFormat.getNumberInstance(Locale.US).format)
					.zipWithIndex
					.map { case (n, i) =>
						val width = calcWidth(i,items.size)
						n.reverse.padTo(width, ' ').reverse
					}.mkString
		)
	}

	// How large the collections will be in each benchmark
	val sizes = Seq(4, 16, 64, 256, 512, 1024, 4096, 16192, 65536, 262144, 1048576)

	@main
	def run( @arg( short = 'e', doc = "coma separated list of benchmarks to execute. Benchmarks: "
												+ "build, remove, concat, foreach, index, contains")
				exec: String = ""
				,@arg( short = 's', doc = "coma separated list of benchmarks not to execute. Benchmarks: "
												+ "build, remove, concat, foreach, index, contains")
				skip: String = ""
				,@arg(short = 't', doc = "How long each benchmark runs, in millisecs")
				time: Int = 2000
				,@arg(short = 'l', doc = "How many times to repeat each benchmark")
				loop: Int = 7
				,@arg(short = 'o', doc = "Name of the result file")
				out: String = defaultResultFileName
			 ): Unit = {
		// How long a benchmark can run before we stop incrementing it
		val cutoff = 400 * 1000 * 1000

		val execute = typesFromString(exec)
		val mustSkip = typesFromString(skip)
		val output = m.Map[(String, String, Long), m.Buffer[Long]]()
		val cutoffSizes = m.Map.empty[(Benchmark.Value, String), Int]
		// Warmups
		val warmupLoops = 10
		val warmupSize = 1024
		println(s"$warmupLoops x $warmupSize warmups...")
		val benchmarks = (if(execute.nonEmpty) Benchmark.benchmarks.filter( b => execute.contains(b.name))
								else Benchmark.benchmarks) filterNot { b => mustSkip.contains(b.name) }
		println(s"Run benchmarks:${benchmarks.map(_.name).mkString(", ")} with time: $time ms into: $out")
		for(benchmark <- benchmarks){
			for (bench <- benchmark.cases){
				def exec[T](bench: Case[T]): Unit = {
					val init = bench.initializer(warmupSize)
					(0 until warmupLoops).foreach { _ =>
						bench.run(init)
					}
				}
				exec(bench)
			}
		}

		// Main loop
		for(i <- 1 to loop) {
			printRow(s"* Run $i / $loop * length:", sizes)
			for(benchmark <- benchmarks){
				println(s"** ${benchmark.name} **")
				for (bench <- benchmark.cases){
					val key = benchmark.name -> bench.name
					val times =
						for(size <- sizes if !(cutoffSizes.getOrElse(key, Int.MaxValue) < size)) yield{
							val buf = output.getOrElseUpdate((benchmark.name.toString, bench.name, size), m.Buffer())
							def handle[T](bench: Case[T] ) = {
								System.gc()
								val init = bench.initializer(size)
								val start = System.currentTimeMillis
								var count = 0
								while(System.currentTimeMillis - start < time){
									var loops = benchmark.name.loops
									while(loops > 0) {
									bench.run(init)
										loops -= 1
									}
									count += 1
								}
								(count, System.currentTimeMillis - start)
							}
							val (runCounts, runTime) = handle(bench)
							val res = (runTime.toDouble * 1000000 / runCounts).toLong
							buf.append(res)
							if (res > cutoff) {
								cutoffSizes(key) = math.min(
									cutoffSizes.getOrElse(key, Int.MaxValue),
									size
								)
							}
							res
						}
					printRow(bench.name, times)
				}
			}
		}

		val resultFile = new File(out)
		Option(resultFile.getParentFile).foreach(_.mkdirs)
		upickle.default.writeToOutputStream(output.mapValues(_.toList).toMap, new FileOutputStream(resultFile))
	}

	def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

	def scalaVersion: String = {
		import java.util.jar.Manifest
		val inputStream = this.getClass.getResourceAsStream("/META-INF/MANIFEST.MF")
		val manifest = new Manifest(inputStream)
		Option(manifest.getMainAttributes.getValue("Scala-Version")).getOrElse("")
	}
}

