package bench

import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale
import bench.Performance.{Out, mapTransposed, sizes}

import java.io.File
import scala.util.Sorting.quickSort

/**
  * Created by haoyi on 9/26/16.
  */
object Analyze {

  def main(args: Array[String]): Unit = {
    val results = upickle.default.read[Out[Seq[Long]]](
      new File( args.headOption.getOrElse( Performance.defaultResultFileName()))
    )
    val grouped =
      results.result.map{ case (bench, rest) =>
          bench -> rest.map{ case (coll, times) =>
              coll -> mapTransposed( times ){ items =>
                  quickSort(items)
                  val divisor = Benchmark.withName(bench).loops
                   var middling = items
                  if(middling.length > 2)	{
                    middling = middling.drop(1).dropRight(1)
                  }
                  middling = middling.map(_ / divisor)
                  val mean = middling.sum / middling.length
                  val stdDev = math.sqrt(middling.map(x => (x-mean) * (x-mean)).sum / middling.length).toLong
                  val accuracy = math.max(1, math.pow(10, math.log10(stdDev).toInt).toInt)
                  val stdDevStr = if (stdDev == 0.0) "0%"
                  else new DecimalFormat("0.0").format(stdDev * 100.0 / math.abs(mean)) + "%"

                  (mean / accuracy * accuracy, stdDevStr)
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
        items.foreach {
          case (mean, stdDev) =>
            print((NumberFormat.getNumberInstance(Locale.US).format(mean) + " ± " + stdDev).reverse.padTo(width, ' ').reverse + " |")
        }
        println()
      }
    }
  }
}
