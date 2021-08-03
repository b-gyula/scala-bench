package bench

import bench.Benchmark.Case

import java.text.NumberFormat
import java.util.Locale
import scala.collection.mutable
import scala.util.control.Exception._

object Performance{
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

	def main(args: Array[String]): Unit = {

		// How many times to repeat each benchmark
		val repeats: Int = args.headOption.flatMap(a => allCatch.opt{a.toInt}).getOrElse(7)

		val resultFile: String = if(args.size > 1) args(1) else "target/results.json"
		// How long each benchmark runs, in millis
		val duration = 2000
		// How long a benchmark can run before we stop incrementing it
		val cutoff = 400 * 1000 * 1000

		val output = mutable.Map.empty[(String, String, Long), mutable.Buffer[Long]]
		val cutoffSizes = mutable.Map.empty[(String, String), Int]
		// Warmups
		val warmupLoops = 20
		val warmupSize = 1024
		println(s"Warmup...")
		for(benchmark <- Benchmark.benchmarks){
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

		printRow("Size", sizes)
		// Main loop
		for(i <- 1 to repeats){
			println(s"** Run $i / $repeats **")
			for(benchmark <- Benchmark.benchmarks){
				println(s"* ${benchmark.name} *")
				for (bench <- benchmark.cases){
					val key = benchmark.name -> bench.name
					val times =
						for(size <- sizes if !(cutoffSizes.getOrElse(key, Int.MaxValue) < size)) yield{
							val buf = output.getOrElseUpdate((benchmark.name, bench.name, size), mutable.Buffer())
							def handle[T](bench: Case[T] ) = {
								System.gc()
								val init = bench.initializer(size)
								val start = System.currentTimeMillis
								var count = 0
								while(System.currentTimeMillis - start < duration){
									bench.run(init)
									count += 1
								}
								val end = System.currentTimeMillis
								(count, end - start)
							}
							val (runCounts, runTime) = handle(bench)
							val res = ((runTime.toDouble / runCounts) * 1000000).toLong
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
		import ammonite.ops._
		write(
			pwd/RelPath(resultFile),
			upickle.default.write(output.mapValues(_.toList).toMap)
		)
	}
}

