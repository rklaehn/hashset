package scala.collection.immutable2

abstract class SetTest[T] {

  def n : Int

  def createElement(i:Int): T

  def empty:Set[T]

  def make(elems:Seq[T]) = empty ++ elems

  def buildTest() {
    val elems = (0 until n).map(createElement _)
    val s = make(elems)
    require(s.size == elems.size)
    require(elems.forall(s.contains _))
  }

  def subsetTest() {
    val elems = (0 until n).map(createElement _)
    val a = make(elems)
    for (i<-0 until n) {
      val b_elems =  (0 until i).map(createElement _)
      val b = make(b_elems)
      require(b.subsetOf(a))
    }
  }

  def intersectTest() {
    val elems = (0 until n).map(createElement _)
    val s = make(elems)
    for (i <- 0 until n) {
      val a = s.take(i)
      val b = s.takeRight(i)
      val c = a.intersect(b)
      require(c == a.filter(b))
    }
  }

  def diffTest() {
    val elems = (0 until n).map(createElement _)
    val s = make(elems)
    for (i <- 0 until n) {
      val a = s.take(i)
      val b = s.takeRight(i)
      val c = a.diff(b)
      require(c == a.filterNot(b))
    }
  }

  def unionTest() {
    val elems = (0 until n).map(createElement _)
    for (i <- 0 until n) {
      val a = make(elems.take(i))
      val b = make(elems.takeRight(i))
      val c = a union b
      require(a.forall(c.contains _))
      require(b.forall(c.contains _))
    }
  }

  def filterTest() {
    val elems = (0 until n).map(createElement _).toIndexedSeq
    val indexOfElem = elems.zipWithIndex.toMap
    val s = make(elems)
    for (i <- 0 until n) {
      val t = s.filter(indexOfElem(_) < i)
      require(t.size == i)
      require(elems.take(i).forall(t.contains _))
    }
  }

  def filterSharingTest() {
    val elems = (0 until n).map(createElement _)
    for (i <- 0 until n) {
      val a = make(elems.take(i))
      require(a.filter(_=>true) eq a)
    }
  }

  def unionSharingTest() {
    val elems = (0 until n).map(createElement _).toIndexedSeq
    val indexOfElem = elems.zipWithIndex.toMap
    val a = make(elems)
    for (i <- 0 until n) {
      val b = a.filter(indexOfElem(_) < i)
      require(a.union(b) eq a)
      require(b.union(a) eq a)
    }
  }

  def diffSharingTest() {
    val elems = (0 until n).map(createElement _)
    for (i <- 0 until n) {
      val a = make(elems.take(i))
      val b = make(elems.drop(i))
      require(a.diff(b) eq a)
    }
  }

  def intersectSharingTest() {
    val elems = (0 until n).map(createElement _).toIndexedSeq
    val indexOfElem = elems.zipWithIndex.toMap
    val a = make(elems)
    for (i <- 0 until n) {
      val b = a.filter(indexOfElem(_) < i)
      require(a.intersect(b) eq b)
      require(b.intersect(a) eq b)
    }
  }

  def allTests() {
    buildTest()
    filterTest()
    subsetTest()
    unionTest()
    intersectTest()
    diffTest()
    filterSharingTest()
    unionSharingTest()
    intersectSharingTest()
    diffSharingTest()
  }
}

object SetTest {
  def apply[T](empty:Set[T], n:Int, createKey:Int=>T) : SetTest[T] = SimpleKeySetTest(empty, n, createKey)

  private case class SimpleKeySetTest[T](empty: Set[T], n: Int, f: Int => T) extends SetTest[T] {
    def createElement(i: Int) = f(i)
  }
}

object HashSetTest extends App {

  val N = 2000

  val empty = HashSet.empty[Any]

  case class Collision(i: Int) { override def hashCode = i / 5 }

  val intTests = SetTest[Any](empty, N, x => x)

  val stringTests = SetTest[Any](empty, N, i => i.toString.reverse.padTo(10, '0').reverse)

  val collisionTests = SetTest[Any](empty, N, i => Collision(i))

  intTests.allTests()

  stringTests.allTests()

  collisionTests.allTests()
}
