package scala.collection.immutable
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import language.postfixOps

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

  def subsetTestPositive() {
    val elems = (0 until n).map(createElement _)
    val a = make(elems)
    for (i<-0 until n) {
      val b_elems =  (0 until i).map(createElement _)
      val b = make(b_elems)
      require(b.subsetOf(a))
    }
  }

  def subsetTestNegative() {
    val elems = (0 until n).map(createElement _)
    val a = make(elems)
    for (i<-0 until n) {
      val b_elems =  (0 until i).map(createElement _)
      val b = make(b_elems)
      require(!a.subsetOf(b))
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
      require(t.size == i, "right size")
      require(elems.take(i).forall(t.contains _), "right elements")
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
    subsetTestPositive()
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

object HashSetTests {

  def N = 2000

  lazy val empty = HashSet.empty[Any]

  case class Collision(i: Int) { override def hashCode = i / 5 }

  lazy val intTests = SetTest[Any](empty, N, x => x)

  lazy val stringTests = SetTest[Any](empty, N, i => i.toString.reverse.padTo(10, '0').reverse)

  lazy val collisionTests = SetTest[Any](empty, N, i => Collision(i))

  lazy val setTests = Seq(intTests, stringTests, collisionTests)

  def all(f:SetTest[Any] => Any) {
    for(test<-setTests)
      f(test)
  }
}

@RunWith(classOf[JUnit4])
class HashSetBulkTest {

  import HashSetTests.all

  @Test
  def testEquality() {
    require(false)
    require(HashSet(1,2) == HashSet(2, 1), "a HashSet should have equality that does not depend on order of elements")
  }

  @Test
  def testBuild() {
    // it should contain all elements that have been added to it, and no others
    all(_.buildTest())
  }

  @Test
  def testFilter() {
    // The filter method should produce sets of just elements that match the given predicate
    all(_.filterTest())
  }

  @Test
  def testFilterSharing() {
    // it should return the original instance instead of an identical copy whenever possible
    all(_.filterSharingTest())
  }

  @Test
  def testSubsetOfPositive() {
    // the subsetOf method should return true for all subsets of the set
    all(_.subsetTestPositive())
  }

  @Test
  def testSubsetOfNegative() {
    // it should return false when called with a subset of the set
    all(_.subsetTestNegative())
  }

  @Test
  def testIntersect() {
    // The intersect method should work identical to the filter method
    all(_.intersectTest())
  }

  @Test
  def testIntersectSharing() {
    // it should return the original instance instead of an identical copy whenever possible
    all(_.intersectSharingTest())
  }

  @Test
  def testDiff() {
    // The diff method should work identical to the filterNot method
    all(_.diffTest())
  }

  @Test
  def testDiffSharing() {
    // it should return the original instance instead of an identical copy whenever possible
    all(_.diffTest())
  }

  @Test
  def testUnion() {
    // The union method" should produce a set that contains all elements of its left and right argument
    all(_.unionTest())
  }

  @Test
  def testUnionSharing() {
    // it should return the original instance instead of an identical copy whenever possible
    all(_.unionSharingTest())
  }
}