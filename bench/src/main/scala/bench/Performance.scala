package bench

import bench.Benchmark.{Case, typesFromString}
import mainargs.{ParserForMethods, arg, main}

import scala.collection.mutable.Buffer
import scala.collection.{immutable, mutable => m}
import java.io.{File, FileOutputStream}
import upickle.default.{ReadWriter, macroRW, writeToOutputStream}

import java.lang.management.ManagementFactory
import scala.{collection => coll}
import java.text.{NumberFormat, SimpleDateFormat}
import java.util.{Date, Locale}
import scala.collection.mutable.{ArrayBuilder, ArraySeq}
import scala.reflect.ClassTag
import scala.util.Sorting.quickSort

object Performance {
	val resultFileName = s"result.$scalaVersion.json"
	def defaultResultFileName(out: String = resultFileName) = "all-" + out

	def calcWidth(i: Int, size: Int): Int = {
		val maxWidth = 19
		maxWidth - ((size - i).toDouble / size * maxWidth / 2).round.toInt
	}

	def printRow[I: Integral](name: String, items: Seq[I], firstColWidth: Int = 21): Unit = {
		println(
			name.padTo(firstColWidth, ' ') +
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

	type Result[A] = coll.Map[String, coll.Map[String, coll.Seq[A]]]
	case class Params(time: Int, loops: Int, scala: String, jvm: String, ver: String, end: String)
	case class Out[A](params: Params, sizes: Seq[Int], result: Result[A])
	implicit val rwParams: ReadWriter[Params] = macroRW
	implicit val rw: ReadWriter[Out[Long]] = macroRW
	implicit val rwSeq: ReadWriter[Out[Seq[Long]]] = macroRW

	@main
	def run( @arg( short = 'e', doc = "coma separated list of benchmarks to execute. Benchmarks: "
												+ "build, remove, concat, foreach, index, contains")
				exec: String = ""
				,@arg( short = 's', doc = "coma separated list of benchmarks not to execute. Benchmarks: "
												+ "build, remove, concat, foreach, index, contains")
				skip: String = ""
				,@arg(short = 't', doc = "How much millisecs each benchmark runs")
				time: Int = 2000
				,@arg(short = 'l', doc = "How many times to repeat each benchmark")
				loop: Int = 7
				,@arg(short = 'o', doc = "Name of the result file")
				out: String = resultFileName
			 ): Unit = {
		// How long a benchmark can run before we stop incrementing it
		val cutoff = 400 * 1000 * 1000

		val execute = typesFromString(exec)
		val mustSkip = typesFromString(skip)
		val output = m.Map[String, m.Map[String, m.Buffer[Seq[Long]]]]()
		val cutoffSizes = m.Map.empty[(Benchmark.Value, String), Int]
		// Warmups
		val warmupLoops = 100
		val warmupSize = 1024
		println(s"$warmupLoops x $warmupSize warmups... PID: $pid")
		val benchmarks = (if(execute.nonEmpty) Benchmark.benchmarks.filter( b => execute.contains(b.name))
								else Benchmark.benchmarks) filterNot { b => mustSkip.contains(b.name) }
		val eta = benchmarks.map(_.cases.length).sum * sizes.length * loop * time / 1000
		println(s"Run benchmarks:${benchmarks.map(_.name).mkString(", ")} with time: $time ms into: $out ETA: " +
					"%dh %dm %ds".format( eta / 3600, (eta % 3600) / 60, eta % 60))

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
			printRow(s"* Run $i / $loop * size:", sizes)
			for(benchmark <- benchmarks){
				println(s"** ${benchmark.name} **")
				for (bench <- benchmark.cases){
					val key = benchmark.name -> bench.name
					val times =
						for(size <- sizes if !(cutoffSizes.getOrElse(key, Int.MaxValue) < size)) yield {
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
							if (res > cutoff) {
								cutoffSizes(key) = math.min(
									cutoffSizes.getOrElse(key, Int.MaxValue),
									size
								)
							}
							res
						}
					output.getOrElseUpdate(benchmark.name.toString, m.Map(bench.name -> Buffer[Seq[Long]]()))
							.getOrElseUpdate(bench.name, Buffer[Seq[Long]]()) += times
					printRow(bench.name, times)
				}
			}
		}
		val prms = Params(time, loop, scalaVersion, jvmVersion, version,
			new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()))
		writeToOutputStream( Out[Seq[Long]]( prms, sizes, output), resultFile( defaultResultFileName( out )) )
		writeToOutputStream( Out[Long]( prms, sizes, aggregate(output)), resultFile(out))

		def resultFile(outFile: String): FileOutputStream = {
			val resultFile = new File(outFile)
			Option(resultFile.getParentFile).foreach(_.mkdirs)
			new FileOutputStream(resultFile)
		}
	}

	def aggregate(result: Result[Seq[Long]]): Result[Long] = {
		result.map { case (bench, cases) =>
			bench -> cases.map { case (caze, times) =>
				val divisor = Benchmark.withName(bench).loops
				caze -> mapTransposed(times)( calcMean(_, divisor))
			}
		}
	}

	def calcMean(res: Array[Long], divisor: Int): Long = {
		quickSort(res)
		var middling = res.view
		if(middling.length > 2)	{
			middling = middling.drop(1).dropRight(1)
		}
		middling.map(_ / divisor).sum / middling.length
	}

	def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

	def fromManifest(value: String): String = {
		import java.util.jar.Manifest
		val inputStream = this.getClass.getResourceAsStream("/META-INF/MANIFEST.MF")
		val manifest = new Manifest(inputStream)
		Option(manifest.getMainAttributes.getValue(value)).getOrElse("")
	}

	def scalaVersion: String = fromManifest( "Scala-Version" )

	def version: String = fromManifest( "Implementation-Version" )

	def jvmVersion: String = System.getProperty("java.version")

	def pid: String = ManagementFactory.getRuntimeMXBean.getName.split('@').headOption.getOrElse("")

	def mapTransposed[T:ClassTag]( runs: coll.Seq[Seq[Long]])( f: Array[Long] => T): Seq[T] = {
		val maxLen = runs.iterator.map(_.length).max
		val r = ArrayBuilder.make[T]
		for(idx <- 0 until maxLen) {
			val times = ArrayBuilder.make[Long]
			runs.foreach { run => if (run.length > idx) times += run(idx) }
			r += f(times.result)
		}
		r.result
	}
}

