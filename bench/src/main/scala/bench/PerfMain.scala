package bench

import java.text.NumberFormat
import java.util.Locale

import scala.collection.{SortedSet, mutable}
import scala.util.control.Exception._

object PerfMain{
  def calcWidth(i: Int, size: Int) = {
    val maxWidth = 15
    maxWidth - ((size - i).toDouble / size * maxWidth / 2).round.toInt
  }

  def printRow[I: Integral](name: String, items: Seq[I]) = {
    println(
      name.padTo(18, ' ') +
         items.map(NumberFormat.getNumberInstance(Locale.US).format)
            .zipWithIndex
            .map { case (n, i) => {
              val width = calcWidth(i,items.size)
              n.reverse.padTo(width, ' ').reverse
            }}.mkString
    )
  }

  // How large the collections will be in each benchmark
  val sizes = Seq(4, 8, 16, 32, 64, 128, 256, 512, 1024, 4096, 16192, 65536, 262144, 1048576)

  def main(args: Array[String]): Unit = {

    // How many times to repeat each benchmark
    val repeats: Int = args.headOption.flatMap(a => allCatch.opt{a.toInt}).getOrElse(7)
    // How long each benchmark runs, in millis
    val duration = 2000
    // How long a benchmark can run before we stop incrementing it
    val cutoff = 400 * 1000 * 1000

    printRow("Size", sizes)
    val output = mutable.Map.empty[(String, String, Long), mutable.Buffer[Long]]
    val cutoffSizes = mutable.Map.empty[(String, String), Int]
    for(i <- 1 to repeats){
      println(s"** Run $i / $repeats **")
      for(benchmark <- Benchmark.benchmarks){
        println()
        println(s"* ${benchmark.name} *")
        println()
        for (bench <- benchmark.cases){
          val key = benchmark.name -> bench.name
          val times =
            for(size <- sizes if !(cutoffSizes.getOrElse(key, Int.MaxValue) < size)) yield{
              val buf = output.getOrElseUpdate((benchmark.name, bench.name, size), mutable.Buffer())
              def handle(run: Boolean) = {
                System.gc()

                val start = System.currentTimeMillis()
                var count = 0
                while(System.currentTimeMillis() - start < duration){
                  if (run) bench.run(size)
                  else bench.initializer(size)
                  count += 1
                }
                val end = System.currentTimeMillis()
                (count, end - start)
              }
              val (initCounts, initTime) = handle(run = false)
              val (runCounts, runTime) = handle(run = true)
              val res = ((runTime.toDouble / runCounts - initTime.toDouble / initCounts) * 1000000).toLong
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
      pwd/'target/"results.json",
      upickle.default.write(output.mapValues(_.toList).toMap)
    )
  }
}

