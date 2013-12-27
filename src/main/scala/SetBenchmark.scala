import scala.collection.immutable._
import scala.collection.immutable2.{HashSet => HashSet2}
import ichi.bench.Thyme

case class SetBenchmark[T](size:Int, offset:Double, elem:Int => T)(empty:Set[T]) {

  val (a,b) = {
    // assignment from empty must not be done in the constructor but in setUp because that
    // is called after the parameters like collectionType are assigned
    var a = empty
    var b = empty
    val k = (size * offset).toInt
    for (i <- 0 until size) {
      a += elem(i)
      b += elem(i + k)
    }
    (a,b)
  }

  def union = a.union(b)

  def intersect = a.intersect(b)

  def diff = a.diff(b)

  def subsetOf = a.subsetOf(b)
}

object SetBenchmark {

  val th = Thyme.warmed()

  case class Collision(x: Int) {
    override def hashCode = x / 3
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
    for {
      size <- Seq(1,10,100, 1000)
      offset <- Seq(0.0, 0.33, 0.66, 1.0)
      keyType <- Seq("int", "string", "vector", "collision")
    }
    {
      val bench = SetBenchmark(size, offset, elem(keyType)) _
      val bench1 = bench(HashSet.empty)
      val bench2 = bench(HashSet2.empty)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op union")(bench2.union)(bench1.union)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op diff")(bench2.diff)(bench1.diff)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op subsetOf")(bench2.subsetOf)(bench1.subsetOf)
      th.pbenchOff(s"size $size offset $offset keyType $keyType op intersect")(bench2.intersect)(bench1.intersect)
    }
  }
}
