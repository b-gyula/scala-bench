package bench

import bench.Performance.printRow

import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.{immutable, mutable}

object Memory{
	def main(args: Array[String]): Unit = {
		val sizes = Seq(0, 1, 4, 16, 64, 256, 1024, 4096, 16192, 65536, 262144, 1048576)
		def obj = new Object()
		def nums[T](n: Int, f: Int => T) = (0 until n).iterator.map(f)
		val collections = Seq[(String, Int => AnyRef)](
			("Array",           nums(_, _ => obj).toArray),
			("List",            nums(_, _ => obj).toList),
			("Vector",          nums(_, _ => obj).toVector),
			("UnforcedStream",  nums(_, _ => obj).toStream),
			("ForcedStream",    {n => val x = nums(n, _ => obj).toStream; x.foreach(x => ()); x}),
			("Set",             nums(_, _ => obj).toSet),
			("Map",             nums(_, _ => (obj, obj)).toMap),

			("SortedSet",   immutable.SortedSet[Int]() ++ nums(_, x=>x)),
			("Queue",       immutable.Queue() ++ nums(_, _ => obj) ),

			("m.Buffer",    nums(_, _ => obj).toBuffer),
			("m.ArrayBuffer", ArrayBuffer() ++ nums(_, _ => obj)),
			("m.ListBuffer", ListBuffer() ++ nums(_, _ => obj)),
			("m.Map",       mutable.Map() ++ nums(_, _ => (obj, obj))),
			("m.Set",       mutable.Set() ++ nums(_, _ => obj)),
			("m.Queue",     mutable.Queue() ++ nums(_, _ => obj)),
			("m.PriQueue",  mutable.PriorityQueue[Int]() ++ nums(_, x=>x)),
			("m.Stack",     mutable.Stack() ++ nums(_, _ => obj)),
			("m.SortedSet", mutable.SortedSet[Int]() ++ nums(_, x=>x)),

			("String",  "1" * _),

			("ArrayBoolean",  nums(_, _ % 2 == 0).toArray),
			("ArrayByte",     nums(_, _.toByte).toArray),
			("ArrayShort",    nums(_, _.toShort).toArray),
			("ArrayInt",      nums(_, _.toInt).toArray),
			("ArrayLong",     nums(_, _.toLong).toArray),

			("BoxArrayBoolean", nums(_, x => (x % 2 == 0).asInstanceOf[AnyRef]).toArray),
			("BoxArrayByte",    nums(_, _.toByte.asInstanceOf[AnyRef]).toArray),
			("BoxArrayShort",   nums(_, _.toShort.asInstanceOf[AnyRef]).toArray),
			("BoxArrayInt",     nums(_, _.toInt.asInstanceOf[AnyRef]).toArray),
			("BoxArrayLong",    nums(_, _.toLong.asInstanceOf[AnyRef]).toArray),

			("j.List",    nums(_, _.toLong.asInstanceOf[AnyRef]).toBuffer.asJava: java.util.List[AnyRef]),
			("j.Map",     n => (mutable.Map() ++ nums(n, _ => (obj, obj))).asJava: java.util.Map[AnyRef, AnyRef]),
			("j.Set",     n => (mutable.Set() ++ nums(n, _ => obj)).asJava: java.util.Set[AnyRef])
		)

		val results = for((name, factory) <- collections) yield {
			val numbers = for(n <- sizes) yield DeepSize(factory(n))
			(name, numbers)
		}

		printRow("Size", sizes)
		println()
		for((name, numbers) <- results){
			printRow(name, numbers)
		}
	}
}

