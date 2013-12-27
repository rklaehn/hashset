import scala.collection.immutable._
import scala.collection.immutable2.{HashSet => HashSet2}
import ichi.bench.Thyme

object SetBenchmark {

  def makeSets[T](size:Int, offset:Double, elem:Int => T, empty:Set[T]) : (Set[T], Set[T]) = {
    var a = empty
    var b = empty
    val k = (size * offset).toInt
    for (i <- 0 until size) {
      a += elem(i)
      b += elem(i + k)
    }
    (a,b)
  }

  val th = Thyme.warmed()

  case class Collision(x: Int) {
    override def hashCode = x / 3
  }

  def empty(setType:String) = setType match {
    case "hashset" => HashSet.empty[Any]
    case "hashset2" => HashSet2.empty[Any]
  }

  def elem(keyType:String)(i: Int) = keyType match {
    case "int" => // a key type with cheap equals and cheap hash code
      i
    case "string" => // a key type with cheap (cached) hash code but expensive equals
      val result = "SomeVeryLongStringSoThatEqualsIsExpensive" + i
      result.hashCode // string caches hashCode, so call it once so we have predictive behavior
      result
    case "vector" => // a key type with relatively expensive hash code and expensive equals
      Vector.fill(15)(0) :+ i
    case "collision" => // a key type that produces lots of collisions
      Collision(i)
    case _ => throw new UnsupportedOperationException("Unknown element type " + keyType)
  }

  def main(args: Array[String]) {

    def timeToString(time:Double) = (time*1000000).formatted("%.3f")

    def ratioToString(r:Double) = r.formatted("%.3f")

    for {
      size <- Seq(1,10,100, 1000)
      offset <- Seq(0.0, 0.33, 0.66, 1.0)
      keyType <- Seq("int", "string", "vector", "collision")
    }
    {
      val (a0, b0) = makeSets(size, offset, elem(keyType), HashSet.empty)
      val (a1, b1) = makeSets(size, offset, elem(keyType), HashSet2.empty)
      val (_, t0) = th.clockPair(a0.subsetOf(b0))
      val (_, t1) = th.clockPair(a1.subsetOf(b1))
      val ratio = t0 / t1
      println(s"$size\t$offset\t$keyType\t${timeToString(t0)}\t${timeToString(t1)}\t${ratioToString(ratio)}")
//      th.clockPair(bench.diff)
//      th.clockPair(bench.subsetOf)
//      th.clockPair(bench.intersect)
    }

    /*
    for {
      size <- Seq(1,10,100, 1000, 10000)
      offset <- Seq(0.0, 0.33, 0.66, 1.0)
      keyType <- Seq("int", "string", "vector", "collision")
    }
    {
      val bench = (x:Set[Any]) => SetBenchmark(size, offset, elem(keyType), x)
      val bench1 = bench(HashSet.empty)
      val bench2 = bench(HashSet2.empty)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op union")(bench2.union)(bench1.union)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op diff")(bench2.diff)(bench1.diff)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op subsetOf")(bench2.subsetOf)(bench1.subsetOf)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op intersect")(bench2.intersect)(bench1.intersect)
    }
    */
  }
}
