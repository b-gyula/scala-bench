package bench

import javax.naming.InvalidNameException
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, ArraySeq, Buffer, ListBuffer}
import scala.language.implicitConversions

case class Benchmark(name: Benchmark.Type,
							cases: Benchmark.Case[_]*) {
	def duplicates = {
		val seen = mutable.Set[String]()
		cases.iterator.map(_.name).filter(n => !seen.add(n))
	}
	if(duplicates.nonEmpty) {
		throw new InvalidNameException(s"Duplicate case name(s) found in $name:" + duplicates.mkString(","))
	}
}

object Benchmark extends Enumeration {
	case class Case[T](name: String,
							 initializer: Int => T)
							(val run: T => Any)

	/** Get Benchmark types (Value) from string */
	def typesFromString(s: String, sep: Char = ','): Array[Type]
				= s.split(sep)
					.map(_.trim)
					.filter(_.nonEmpty)
					.map(s => value2Type(Benchmark.withName(s)))

	implicit def value2Type(x: Value): Type = x.asInstanceOf[Type]

	case class Type(loops: Int = 1) extends super.Val
	val build, remove, concat = Type()
	val loop = Type(10)
	val index = Type(100)
	val contains = Type(1)

	def pair[T](t: => T) = (t, t)

	val nullO: Object = null

	@inline def obj = new Object()

	/** Keep object Array for each size */ // TODO just create one array and use iterator
	val _objArray: mutable.Map[Int, ArraySeq[Object]] = mutable.Map()

	@inline
	def objs(i: Int): collection.Seq[Object] = _objArray.getOrElseUpdate(i, ArraySeq.fill(i)(obj))
	def arraySeq(i: Int) = objs(i).asInstanceOf[ArraySeq[Object]]
	val _listBuffers = mutable.Map[Int, ListBuffer[Object]]()
	def list(i: Int): List[Object] = listBuffer(i).toList
	def listBuffer(i: Int) = _listBuffers.getOrElseUpdate(i, ListBuffer[Object]() ++= objs(i))

	val _vectors = mutable.Map[Int, Vector[Object]]()
	def vector(i: Int): Vector[Object] = _vectors.getOrElseUpdate(i, objs(i).toVector)

	val _msets = mutable.Map[Int, mutable.Set[Object]]()
	def mSet(i: Int) = _msets.getOrElseUpdate(i, mutable.Set[Object]() ++= objs(i))

	val _sets = mutable.Map[Int, Set[Object]]()
	def set(i: Int) = _sets.getOrElseUpdate(i, objs(i).toSet)

	val _arrayBuffers = mutable.Map[Int, ArrayBuffer[Object]]()
	def arrayBuffer(i: Int) = _arrayBuffers.getOrElseUpdate(i, ArrayBuffer[Object]() ++= objs(i))

	def array(i: Int): Array[Object] = objs(i).asInstanceOf[ArraySeq[Object]].array.asInstanceOf[Array[Object]]

	val _mmaps = mutable.Map[Int, mutable.Map[Object, Int]]()
	def mMap(i: Int) = _mmaps.getOrElseUpdate(i, mutable.Map() ++= objs(i).map(x => x -> 1))

	val _maps = mutable.Map[Int, Map[Object, Int]]()
	def map(i: Int) = _maps.getOrElseUpdate(i, objs(i).map(x => x -> 1).toMap)

	@inline def loopFor(it: Iterable[Object]) = {
		var last = nullO
		for(j <- it) {
			last = j
		}
		last
	}
	
	@inline def loopForeach(it: Iterable[Object]) = {
		var last = nullO
		it.foreach(last = _)
		last
	}
	
	@inline def loopWhile(it: collection.Seq[Object]) = {
		var last = nullO
		var j = 0
		while(j < it.length) {
			last = it(j)
			j += 1
		}
		last
	}

	val benchmarks = Seq(
		Benchmark( build,
			Case("List::", n => n) { n =>
				var b = List.empty[Object]
				var i = 0
				while(i < n) {
					b = obj :: b
					i += 1
				}
				b
			},
			Case("List:+", n => n) { n =>
				var b = List.empty[Object]
				var i = 0
				while(i < n) {
					b = b :+ obj
					i += 1
				}
				b
			},
			Case("List+:", n => n) { n =>
				var b = List.empty[Object]
				var i = 0
				while(i < n) {
					b = obj +: b
					i += 1
				}
				b
			},
			Case("m.Buffer", n => n) { n =>
				val b = Buffer[Object]()
				var i = 0
				while(i < n) {
					b.append(obj)
					i += 1
				}
				b
			},
			Case("m.Buffer.empty", n => n) { n =>
				val b = Buffer.empty[Object]
				var i = 0
				while(i < n) {
					b.append(obj)
					i += 1
				}
				b
			},
			Case("m.Buffer+toSeq", n=>n){ n =>
				val b = Buffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toSeq
			},
			Case("m.Buffer+toArray", n=>n){ n =>
				val b = Buffer.empty[Object]
				var i = 0
				while(i < n){
					b += obj
					i += 1
				}
				b.toArray
			},
			Case("m.ListBuffer", n => n){ n =>
				val b = ListBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b
			},
			Case("m.ListBuffer+toSeq", n => n ){ n =>
				val b = ListBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toSeq
			},
			Case("m.ListBuffer+toArray", n => n ){ n =>
				val b = ListBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toArray
			},
			Case("m.ArrayBuilder", n => n){ n =>
				val b = ArrayBuilder.make[Object]
				var i = 0
				while(i < n){
					b += obj
					i += 1
				}
				b
			},
			Case("m.ArrayBuffer", n => n){ n =>
				val b = ArrayBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b
			},
			Case("m.ArrayBuffer(size)", n => n){ n =>
				val b = new ArrayBuffer[Object](n)
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b
			},
			Case("m.ArraySeq:+", n => n){ n =>
				var b = mutable.ArraySeq.empty[Object]
				var i = 0
				while(i < n){
					b = b :+ obj
					i += 1
				}
				b
			},/****** Does not compile 2.13 + *********/
			Case("m.ArraySeq.update", n => n){ n =>
				val b = new mutable.ArraySeq[Object](n)
				var i = 0
				while(i < n){
					b.update(i, obj)
					i += 1
				}
				b
			},/********/
			Case("m.ArraySeq+:", n => n){ n =>
				var b = mutable.ArraySeq.empty[Object]
				var i = 0
				while(i < n){
					b = obj +: b
					i += 1
				}
				b
			},
			Case("m.ArrayBuffer+toSeq", n => n ){ n =>
				val b = ArrayBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toSeq
			},
			Case("m.ArrayBuffer+toArray", n => n ){ n =>
				val b = ArrayBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toArray
			},
			Case("Vector", n => n) { n =>
				var b = Vector.empty[Object]
				var i = 0
				while(i < n) {
					b = obj +: b
					i += 1
				}
				b
			},
			Case("m.VectorBuilder", n => n){ n =>
				val b = Vector.newBuilder[Object]
				var i = 0
				while(i < n){
					b += obj
					i += 1
				}
				b
			},
			Case("Array:+", n => n) { n =>
				var b = new Array[Object](0)
				var i = 0
				while(i < n) {
					b = b :+ obj
					i += 1
				}
				b
			},
			Case("Array(size)", n => n) { n =>
				val b = new Array[Object](n)
				var i = 0
				while(i < n) {
					b(i) = obj
					i += 1
				}
				b
			},
			Case("Array(size)+toBuffer", n => n) { n =>
				val b = new Array[Object](n)
				var i = 0
				while(i < n) {
					b(i) = obj
					i += 1
				}
				b.toBuffer
			},
			Case("Array(size)+toVector", n => n) { n =>
				val b = new Array[Object](n)
				var i = 0
				while(i < n) {
					b(i) = obj
					i += 1
				}
				b.toVector
			},
			Case("Array(size)+toSet", n => n) { n =>
				val b = new Array[Object](n)
				var i = 0
				while(i < n) {
					b(i) = obj
					i += 1
				}
				b.toSet
			},
			Case("Array(size)+toMap", n => n) { n =>
				val b = new Array[(Object, Object)](n)
				var i = 0
				while(i < n) {
					b(i) = (obj, obj)
					i += 1
				}
				b.toMap
			},
			Case("Set", n => n) { n =>
				var b = Set.empty[Object]
				var i = 0
				while(i < n) {
					b = b + obj
					i += 1
				}
				b
			},
			Case("Map", n => n) { n =>
				var b = Map.empty[Object, Object]
				var i = 0
				while(i < n) {
					b = b + (obj -> obj)
					i += 1
				}
				b
			},
			Case("m.Set", n => n) { n =>
				val b = mutable.Set.empty[Object]
				var i = 0
				while(i < n) {
					b.add(obj)
					i += 1
				}
				b
			},
			Case("m.Map", n => n) { n =>
				val b = mutable.Map.empty[Object, Object]
				var i = 0
				while(i < n) {
					b.put(obj, obj)
					i += 1
				}
				b
			}
		),
		Benchmark( remove,
			Case("List.tail", list) { a =>
				var x = a
				while(x.nonEmpty) x = x.tail
				x
			},
			Case("List.init", list) { a =>
				var x = a
				while(x.nonEmpty) x = x.init
				x
			},
			Case("Vector.tail", vector) { a =>
				var x = a
				while(x.nonEmpty) x = x.tail
				x
			},
			Case("Vector.init", vector) { a =>
				var x = a
				while(x.nonEmpty) x = x.init
				x
			},
			Case("Set.-", set) { a =>
				var x = a
				while(x.nonEmpty) x = x - x.head
				x
			},
			Case("Map.-", map) { a =>
				var x = a
				while(x.nonEmpty) x = x.-(x.head._1)
				x
			},
			Case("Array.tail", array) { a =>
				var x = a
				while(x.nonEmpty) x = x.tail
				x
			},
			Case("m.(Array)Buffer", arrayBuffer(_).clone) { a =>
				while(a.nonEmpty) a.remove(a.length - 1)
				a
			},
			Case("m.ListBuffer", listBuffer(_).clone) { a =>
				while(a.nonEmpty) a.remove(a.length - 1)
				a
			},
			Case("m.Set", mSet(_).clone) { a =>
				while(a.nonEmpty) a.remove(a.head)
				a
			},
			Case("m.Map", mMap(_).clone) { a =>
				while(a.nonEmpty) a.remove(a.head._1)
				a
			}
		),
		Benchmark( concat,
			Case("List", x => pair(list(x))){ case (a, b) =>
				a ++ b
			},
			Case("Vector", x => pair(vector(x))) { case (a, b) =>
				a ++ b
			},
			Case("Set", x => set(x) -> Array.fill(x)(obj).toSet) { case (a, b) =>
				a ++ b
			},
			Case("Set-same", x => pair(set(x))) { case (a, b) =>
				a ++ b
			},
			Case("Map", x => map(x) -> Array.fill(x)(obj -> 1).toMap) { case (a, b) =>
				a ++ b
			},
			Case("Map-same", x => pair(map(x))) { case (a, b) =>
				a ++ b
			},
			Case("Array++", x => pair(array(x))) { case (a, b) =>
				a ++ b
			},
			Case("m.ArraySeq", x => pair(objs(x))) { case (a, b) =>
				a ++ b
			},
			Case("Array-arraycopy", x => pair(array(x))) { case (a, b) =>
				val x = new Array[Object](a.length + b.length)
				System.arraycopy(a, 0, x, 0, a.length)
				System.arraycopy(b, 0, x, a.length, b.length)
				x
			},
			Case("m.(Array)Buffer", x => pair(arrayBuffer( x ))) { case (a, b) =>
				a ++ b
			},
			Case("m.ListBuffer", x => pair( listBuffer( x ) )) { case (a, b) =>
				a ++ b
			},
			Case("m.Set", x => (mutable.Set[Object]() ++= objs(x), mSet( x ))) { case (a, b) =>
				a ++ b
			},
			Case("m.Set-same", x => pair(mSet( x ))) { case (a, b) =>
				a ++ b
			},
			Case("m.Map-same", x => pair(mMap(x))) { case (a, b) =>
				a ++ b
			},
			Case("m.Map", x => (mutable.Map() ++= Array.fill(x)(obj -> 1) ) -> mMap(x) ) { case (a, b) =>
				a ++= b
			}
		),
		Benchmark( loop,
			Case("List-foreach", list) (loopForeach),
			Case("List-for", list) (loopFor),
			Case("List-while-head", list) { a =>
				var last = nullO
				var j = a
				while(j.nonEmpty) {
					last = j.head
					j = j.tail
				}
				last
			},

			Case("Vector-foreach", vector)(loopForeach),
			Case("Vector-for", vector)(loopFor),
			Case("Vector-while", vector)(loopWhile),

			Case("Set-foreach", set) (loopForeach),
			Case("Set-for", set) (loopFor),

			Case("Map-foreach", map) (loopForeach),
			Case("Map-for", map) (loopFor),

			Case("Array-foreach", array) { a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("Array-for", array){ a =>
				var last = nullO
				for(j <- a ) {
					last = j
				}
				last
			},
			Case("Array-while", array) { a =>
				var last = nullO
				var j = a.length
				while(j > 0 ) {
					j -= 1
					last = a(j)
				}
				last
			},
			Case("m.ArraySeq-foreach", objs)(loopForeach),
			Case("m.ArraySeq-for", objs)(loopFor),
			Case("m.ArraySeq-while", objs)(loopWhile),
/*
			Case("m.Buffer-foreach", buffer)(loopForeach),
			Case("m.Buffer-for", buffer) (loopFor),
			Case("m.Buffer-while", buffer) (loopWhile),*/

			Case("m.(Array)Buffer-foreach", arrayBuffer) (loopForeach),
			Case("m.(Array)Buffer-for", arrayBuffer) (loopFor),
			Case("m.(Array)Buffer-while", arrayBuffer) (loopWhile),

			Case("m.ListBuffer-foreach", listBuffer) (loopForeach),
			Case("m.ListBuffer-for", listBuffer) (loopFor),
			Case("m.ListBuffer-while", listBuffer) (loopWhile),

			Case("m.Set-foreach", mSet) (loopForeach),
			Case("m.Set-for", mSet)(loopFor),

			Case("m.Map-foreach", mMap) (loopForeach),
			Case("m.Map-for", mMap)(loopFor)
		),
		Benchmark( index,
			Case("Array", x => x -> array(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.ArraySeq", x => x -> arraySeq(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.ArraySeq-loop", arraySeq) { loopWhile },
			Case("m.(Array)Buffer", x => x -> arrayBuffer(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("Vector", x => x -> vector(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("List", x => x -> list(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
	/*		Case("m.Buffer", x => x -> buffer(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},*/
			Case("m.ListBuffer", x => x -> listBuffer(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			}
		),
		Benchmark( contains,
			Case("List", x =>
				objs(x) -> list(x)) { seqContainsIt
			},
			Case("Vector", x =>
				objs(x) -> vector(x)){ seqContainsIt
			},
			Case("m.ArraySeq-while", x =>
				objs(x) -> arraySeq(x)){ seqContains
			},
			Case("m.ArraySeq", x =>
				objs(x) -> arraySeq(x)){ seqContainsIt
			},
			Case("Array", x =>
				objs(x) -> array(x)
			) { case (keys, a) =>
				var last = false
				val i = keys.iterator
				while(i.hasNext) {
					last = a.contains(i.next)
				}
				last
			},
			Case("m.(Array)Buffer", x =>
				objs(x) -> arrayBuffer(x)){ seqContainsIt
			},
			Case("m.ListBuffer", x =>
				objs(x) -> listBuffer(x)){ seqContainsIt
			},
			Case("Set", x =>
				objs(x) -> set(x)
			){ case (keys, a) =>
				var last = false
				val i = keys.iterator
				while(i.hasNext) {
					last = a.contains(i.next)
				}
				last
			},
			Case("m.Set", x =>
				objs(x) -> mSet(x)
			){ case (keys, a) =>
				var last = false
				val i = keys.iterator
				while(i.hasNext) {
					last = a.contains(i.next)
				}
				last
			},
			Case("Map", x =>
				objs(x) -> map(x)
			){ case (keys, a) => mapContains(keys,a)
			},
			Case("m.Map", x =>
				objs(x) -> mMap(x)
			){ case (keys, a) => mapContains(keys,a)
			}
		)
	)

	@inline def mapContains [T](keys: collection.Seq[Object], a: collection.Map[Object, T]) = {
		var last = false
		val it = keys.iterator
		while(it.hasNext) {
			last = a.contains(it.next)
		}
		last
	}

	@inline private def seqContains(p: ( collection.Seq[Object], collection.Seq[Object])) = p match { case (keys, a) =>
		var last = false
		var j = 0
		while(j < keys.length) {
			last = a.contains(keys(j))
			j += 1
		}
		last
	}

	@inline private def seqContainsIt(p: ( Iterable[Object], collection.Seq[Object])) = p match { case (keys, a) =>
		var last = false
		val it = keys.iterator
		while(it.hasNext) {
			last = a.contains(it.next)
		}
		last
	}
}
