package bench

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder, ArraySeq, Buffer, ListBuffer}
import scala.language.implicitConversions

case class Benchmark(name: Benchmark.Type,
							cases: Benchmark.Case[_]*)

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
	val foreach = Type(10)
	val index = Type(100)
	val contains = Type(10)

	def pair[T](t: => T) = (t, t)

	val nullO: Object = null

	@`inline` def obj = new Object()

	/** Keep object Array for each size */
	val _objArray: mutable.Map[Int, ArraySeq[Object]] = mutable.Map()

	@`inline` def objArray(i: Int): Array[Object] = objs(i).array.asInstanceOf[Array[Object]]

	def objs(i: Int): ArraySeq[Object] = _objArray.getOrElseUpdate(i, ArraySeq.fill(i)(obj))

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
			Case("Buffer", n => n) { n =>
				val b = Buffer[Object]()
				var i = 0
				while(i < n) {
					b.append(obj)
					i += 1
				}
				b
			},
			Case("Buffer.empty", n => n) { n =>
				val b = Buffer.empty[Object]
				var i = 0
				while(i < n) {
					b.append(obj)
					i += 1
				}
				b
			},
			Case("Buffer+toSeq", n=>n){ n =>
				val b = Buffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toSeq
			},
			Case("Buffer+toArray", n=>n){ n =>
				val b = Buffer.empty[Object]
				var i = 0
				while(i < n){
					b += obj
					i += 1
				}
				b.toArray
			},
			Case("ListBuffer", n => n){ n =>
				val b = ListBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b
			},
			Case("ListBuffer+toSeq", n => n ){ n =>
				val b = ListBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toSeq
			},
			Case("ListBuffer+toArray", n => n ){ n =>
				val b = ListBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toArray
			},
			Case("ArrayBuilder", n => n){ n =>
				val b = ArrayBuilder.make[Object]
				var i = 0
				while(i < n){
					b += obj
					i += 1
				}
				b
			},
			Case("ArrayBuffer", n => n){ n =>
				val b = ArrayBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b
			},
			Case("ArrayBuffer(size)", n => n){ n =>
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
			},
			Case("m.ArraySeq.update", n => n){ n =>
				var b = new mutable.ArraySeq[Object](n)
				var i = 0
				while(i < n){
					b.update(i, obj)
					i += 1
				}
				b
			},
			Case("m.ArraySeq+:", n => n){ n =>
				var b = mutable.ArraySeq.empty[Object]
				var i = 0
				while(i < n){
					b = obj +: b
					i += 1
				}
				b
			},
			Case("ArrayBuffer+toSeq", n => n ){ n =>
				val b = ArrayBuffer.empty[Object]
				var i = 0
				while(i < n){
					b.append(obj)
					i += 1
				}
				b.toSeq
			},
			Case("ArrayBuffer+toArray", n => n ){ n =>
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
		Benchmark( remove, // All initializers has to create o copy if using objArray
			Case("List.tail", objs(_).toList) { a =>
				var x = a
				while(x.nonEmpty) x = x.tail
				x
			},
			Case("Vector.tail", objs(_).toVector) { a =>
				var x = a
				while(x.nonEmpty) x = x.tail
				x
			},
			Case("Vector.init", objs(_).toVector) { a =>
				var x = a
				while(x.nonEmpty) x = x.init
				x
			},
			Case("Set.-", objs(_).toSet) { a =>
				var x = a
				while(x.nonEmpty) x = x - x.head
				x
			},
			Case("Map.-", Array.fill(_)(obj -> obj).toMap) { a =>
				var x = a
				while(x.nonEmpty) x = x.-(x.head._1)
				x
			},
			Case("Array.tail", x => Array(objs(x):_*)) { a =>
				var x = a
				while(x.nonEmpty) x = x.tail
				x
			},//								Buffer ++= adds items one by one
			Case("m.Buffer", x => Buffer( objs(x):_* )) { a =>
				while(a.nonEmpty) a.remove(a.length - 1)
				a
			},
			Case("m.Set", x => mutable.Set(objs(x):_*)) { a =>
				while(a.nonEmpty) a.remove(a.head)
				a
			},
			Case("m.Map", x => mutable.Map(Seq.fill(x)(obj -> obj):_*)) { a =>
				while(a.nonEmpty) a.remove(a.head._1)
				a
			}
		),
		Benchmark( concat,
			Case("List", x => pair(objs(x).toList)){ case (a, b) =>
				a ++ b
			},
			Case("Vector", x => pair(objs(x).toVector)) { case (a, b) =>
				a ++ b
			},
			Case("Set", x => (objs(x).toSet, Array.fill(x)(obj).toSet)) { case (a, b) =>
				a ++ b
			},
			Case("Map", x => pair(Array.fill(x)(obj -> obj).toMap)) { case (a, b) =>
				a ++ b
			},
			Case("Array++", x => pair(objArray(x))) { case (a, b) =>
				a ++ b
			},
			Case("m.ArraySeq", x => pair(objs(x))) { case (a, b) =>
				a ++ b
			},
			Case("Array-arraycopy", x => pair(objArray(x))) { case (a, b) =>
				val x = new Array[Object](a.length + b.length)
				System.arraycopy(a, 0, x, 0, a.length)
				System.arraycopy(b, 0, x, a.length, b.length)
				x
			},
			Case("m.Buffer", x => pair(Buffer( objs(x):_*) )) { case (a, b) =>
				a.++= (b)
			},
			Case("m.ArrayBuffer", x => pair(ArrayBuffer( objs(x):_*) )) { case (a, b) =>
				a.++= (b)
			},
			Case("m.ListBuffer", x => pair(ListBuffer( objs(x):_*) )) { case (a, b) =>
				a.++= (b)
			},
			Case("m.Set", x => pair(mutable.Set( objs(x):_*))) { case (a, b) =>
				a ++= b
			},
			Case("m.Map", x => pair(mutable.Map() ++= Array.fill(x)(obj -> obj))) { case (a, b) =>
				a ++= b
			}
		),
		Benchmark( foreach,
			Case("List", objs(_).toList) { a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("List-while", objs(_).toList) { a =>
				var last = nullO
				var j = a
				while(j.nonEmpty) {
					last = j.head
					j = j.tail
				}
				last
			},
			Case("List-for", objs(_).toList) { a =>
				var last = nullO
				for(j <- a) {
					last = j
				}
				last
			},
			Case("Vector", objs(_).toVector){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("Set", objs(_).toSet) { a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("Map", Array.fill(_)(obj -> obj).toMap){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("Array", objArray) { a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("Array-while", n => n -> objArray(n)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.ArraySeq", objs){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("m.ArraySeq-for", objs){ a =>
				var last = nullO
				for(j <- a ) {
					last = j
				}
				last
			},
			Case("m.Buffer", x => Buffer(objs(x):_*)){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("m.ArrayBuffer", x => ArrayBuffer(objs(x):_*)){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("m.Set", x => mutable.Set(objs(x):_*)){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			},
			Case("m.Map", mutable.Map() ++= Array.fill(_)(obj -> obj)){ a =>
				var last = nullO
				a.foreach(last = _)
				last
			}
		),
		Benchmark( index,
			Case("Array", x => x -> objArray(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.ArraySeq", x => x -> objs(x)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.ArrayBuffer", x => x -> ArrayBuffer( objs(x):_*)) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("Vector", x => x -> objs(x).toVector) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("List", x => x -> objs(x).toList) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.Buffer", x => x -> objs(x).toBuffer) { case (n, a) =>
				var last = nullO
				var j = 0
				while(j < n) {
					last = a(j)
					j += 1
				}
				last
			},
			Case("m.ListBuffer", x => x -> ListBuffer( objs(x):_*)) { case (n, a) =>
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
			Case("List", x => {
				val r = objs(x)
				r -> r.toList
			}) { case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("Vector", x => {
				val r = objs(x)
				r -> r.toVector
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.ArraySeq", x => {
				val r = objs(x)
				r -> r
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.ArraySeq-for", x => {
				val r = objs(x)
				r -> r
			}){ case (keys, a) =>
				var last = false
				for( j <- keys) {
					last = a.contains(j)
				}
				last
			},
			Case("Array", x =>
				objs(x) -> objArray(x)
			) { case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.Buffer", x => {
				val r = objs(x)
				r -> r.toBuffer
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.ArrayBuffer", x => {
				val r = objs(x)
				r -> ArrayBuffer( r:_* )
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.ListBuffer", x => {
				val r = objs(x)
				r -> ListBuffer( r:_* )
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("Set", x => {
				val r = objs(x)
				r -> r.toSet
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.Set", x => {
				val r = objs(x)
				r -> mutable.Set( r:_* )
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("Map", x => {
				val r = Array.fill(x)(obj -> obj).toMap
				r.keysIterator.toArray -> r
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			},
			Case("m.Map", x => {
				val r = mutable.Map(Seq.fill(x)(obj -> obj):_*)
				r.keysIterator.toArray -> r
			}){ case (keys, a) =>
				var last = false
				var j = keys.length
				while(j > 0) {
					j -= 1
					last = a.contains(keys(j))
				}
				last
			}
		)
	)
}
