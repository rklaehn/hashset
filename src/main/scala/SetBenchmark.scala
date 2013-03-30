import com.google.caliper._
import scala.collection.immutable._
import scala.collection.immutable2.{HashSet => HashSet2}

class SetBenchmark extends SimpleBenchmark {
  /**
   * The size of the set being tested
   */
  @Param(Array("2", "10", "100", "1000")) var size: Int = 0
  /**
   * The "overlap factor" for various operations. 0 means full overlap, 1 means no overlap
   */
  @Param(Array("0", "0.33", "0.66", "1")) var offset: Double = 0.0
  /**
   * The key type.
   * - int has fast hashcode and equals,
   * - string has fast (cached) hashcode but slow equals
   * - vector has expensive hashcode and equals
   * - collision is a type that is designed to produce many collisions
   */
  @Param(Array("int", "string", "vector", "collision")) var keyType: String = "int"
  /**
   * The type to test.
   * - hashset is the s.c.i.HashSet from the scala distribution
   * - hashset2 is the new, hopefully faster, HashSet from s.c.i2
   */
  @Param(Array("hashset2", "hashset")) var collectionType: String = "hashset2"

  case class Collision(x: Int) {
    override def hashCode = x / 3
  }

  def empty = collectionType match {
    case "hashset" => HashSet.empty[Any]
    case "hashset2" => HashSet2.empty[Any]
    case x => throw new UnsupportedOperationException("Unknown collection type " + x)
  }

  def elem(i: Int) = keyType match {
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

  var a: Set[Any] = null
  var b: Set[Any] = null

  protected override def setUp() {
    // assignment from empty must not be done in the constructor but in setUp because that
    // is called after the parameters like collectionType are assigned
    a = empty
    b = empty
    val k = (size * offset).toInt
    for (i <- 0 until size) {
      a += elem(i)
      b += elem(i + k)
    }
  }

  def timeUnion(reps: Int) = {
    var i = 0
    var result = a
    while (i < reps) {
      result = a.union(b)
      i += 1
    }
    result
  }

  def timeIntersect(reps: Int) = {
    var i = 0
    var result = a
    while (i < reps) {
      result = a.intersect(b)
      i += 1
    }
    result
  }

  def timeDiff(reps: Int) = {
    var i = 0
    var result = a
    while (i < reps) {
      result = a.diff(b)
      i += 1
    }
    result
  }

  def timeSubsetOf(reps: Int) = {
    var i = 0
    var result = false
    while (i < reps) {
      result = a.subsetOf(b)
      i += 1
    }
    result
  }
}

object SetBenchmark {
  def main(args: Array[String]) {
    Runner.main(classOf[SetBenchmark], args)
  }
}
