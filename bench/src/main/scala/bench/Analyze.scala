package bench

import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale
import bench.Performance.sizes

import java.io.File

/**
  * Created by haoyi on 9/26/16.
  */
object Analyze {
  def main(args: Array[String]): Unit = {
    val results = upickle.default.read[Map[(String, String, Long), Vector[Long]]](
      new File(args.headOption.getOrElse(Performance.defaultResultFileName))
    )
    val grouped: Map[String, Map[String, Map[Long, (Long, String)]]] = {
      results.groupBy{case ((bench, coll, size), res) => bench }
        .map{ case (bench, rest) =>
          bench -> rest.groupBy{case ((bench, coll, size), res) => coll}
            .map{ case (coll, rest) =>
              coll -> rest.groupBy { case ((bench, coll, size), res) => size }
                .mapValues{ items =>
                  val divisor = Benchmark.withName(bench).loops
                  val sorted = items.toVector.flatMap{case ((bench, coll, size), res) => res}.sorted
                  val middling = sorted.drop(1).dropRight(1).map(_ / divisor)
                  val mean = middling.sum / middling.length
                  val stdDev = math.sqrt(middling.map(x => (x-mean) * (x-mean)).sum / middling.length).toLong
                  val accuracy = math.max(1, math.pow(10, math.log10(stdDev).toInt).toInt)

                  val stdDevStr = if (stdDev == 0.0) "0%"
                  else new DecimalFormat("0.0").format(stdDev * 100.0 / math.abs(mean)) + "%"

                  (mean / accuracy * accuracy, stdDevStr)
                }
            }
        }
    }

    val width = 15
    pprint.pprintln(grouped)
    print("|:" + "-" * width + "-|")
    for(size <- sizes){
      print("-"*width + ":|")
    }
    println()
    for((bench, items) <- grouped){
      print("| " + " "*width + " |")
      for(size <- sizes){
        print(" "*width + " |")
      }
      println()
      print("| " +("**"+bench+"**").padTo(width, ' ') + " |")
      for(size <- sizes){
        print(("**" + NumberFormat.getNumberInstance(Locale.US).format(size) + "**").reverse.padTo(width, ' ').reverse + " |")
      }
      println()
      print("| " + " "*width + " |")
      for(size <- sizes){
        print(" "*width + " |")
      }
      println()

      for((coll, items) <- items){
        print("| ")
        print(coll.padTo(width, ' '))
        print(" |")
        for(size <- sizes){
          items.get(size) match{
            case Some((mean, stdDev)) =>
//              val ranges = Seq(
//                1000000000 -> "s",
//                1000000 -> "ms",
//                1000 -> "us",
//                1 -> "ns"
//              )
//              val (div, suffix) = ranges.find(_._1 < math.abs(mean)).getOrElse(1 -> "ns")
              val (div, suffix) = (1, "")
//              val mathContext = new MathContext(2, RoundingMode.DOWN)
//              val bigDecimal = new java.math.BigDecimal(mean * 1.0 / div, mathContext)

//              print((bigDecimal.toPlainString() + suffix + " ± " + stdDev).reverse.padTo(width, ' ').reverse + " |")
              print((NumberFormat.getNumberInstance(Locale.US).format(mean) + " ± " + stdDev).reverse.padTo(width, ' ').reverse + " |")
            case None =>
              print(" " * width + " |")
          }

        }
        println()
      }

    }
  }
}
