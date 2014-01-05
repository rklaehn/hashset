import scala.collection.immutable.ListSet
import scala.collection.immutable4.HashSet

object DiffDebug extends App {

  var hashCount = 0

  case class Collision(value: Int) {

    override def hashCode = {
      hashCount += 1
      value / 5
    }
  }

  case class HashCounter(value: Int) {

    override def hashCode = {
      hashCount += 1
      value
    }
  }

  def testDiff[T](sizes: Seq[Int], offsets: Seq[Double], keyType: String, mkKey: Int => T) {
    for {
      i <- sizes
      o <- offsets
    } {
      val e = HashSet.empty[T]
      val j = (i * o).toInt
      // create two sets of size i with overlap o
      val a = e ++ (0 until i).map(mkKey)
      require(a.size == i, s"Building HashSet of size $i failed. Key type $keyType.")
      val b = e ++ (j until (i + j)).map(mkKey)
      require(b.size == i, s"Building HashSet of size $i failed. Key type $keyType.")
      val as = e ++ (0 until j).map(mkKey)
      require(as.size == j, s"Building HashSet of size $j failed. Key type $keyType.")
      val hashCount0 = hashCount
      val u = a diff b
      require(u == (a diff ListSet(b.toSeq: _*)), s"Diff must still work for non-HashSet sets!")
      require(hashCount == hashCount0, s"Bug in diff. key.hashCode should not be called during diff, but has been called ${hashCount - hashCount0} times. Key type $keyType.")
      require(u.size == j, s"Bug in diff. Expected size $j. Real size ${u.size}. Key type $keyType.")
      for (x <- 0 until j)
        require(u.contains(mkKey(x)), s"Bug in diff. Key type $keyType. Set (0 until ${i + j}) should contain $x but does not.")
      require((as intersect b).isEmpty)
      val b_as = b diff as
      val as_b = as diff b
      require((b_as eq b) || (b_as eq as), s"No structural sharing in b diff as. Key type $keyType, b=($j until ${i + j}) as=(0 until $j)")
      require((as_b eq b) || (as_b eq as), s"No structural sharing in as diff b. Key type $keyType, b=($j until ${i + j}) as=(0 until $j)")
    }
  }

  val sizes = Seq(1, 10, 100, 1000, 10000, 100000)
  val offsets = Seq(0.0, 0.25, 0.5, 0.75, 1.0)
  testDiff(sizes, offsets, "int", identity[Int])
  testDiff(sizes, offsets, "hashCounter", HashCounter.apply)
  testDiff(sizes, offsets, "collision", Collision.apply)
}