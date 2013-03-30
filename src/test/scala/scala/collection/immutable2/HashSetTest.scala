package scala.collection.immutable2
import language.postfixOps
import org.scalatest._
import matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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

@RunWith(classOf[JUnitRunner])
class HashSetSpec extends FlatSpec with ShouldMatchers {

  import HashSetTests.all

  "a HashSet" should "have equality that does not depend on order of elements" in {
    HashSet(1,2) == HashSet(2, 1)
//    Rational(4,2) should equal (Rational(2))
//    Rational(9,6) should equal (Rational(3,2))
  }

  it should "contain all elements that have been added to it, and no others" in all(_.buildTest())

  "The filter method" should "produce sets of just elements that match the given predicate" in all(_.filterTest())

  it should "return the original instance instead of an identical copy whenever possible" in all(_.filterSharingTest())

  "the subsetOf method" should "return true for all subsets of the set" in all(_.subsetTestPositive())

  it should "return false when called with a subset of the set" in all(_.subsetTestNegative())

  "The intersect method" should "work identical to the filter method" in all(_.intersectTest())

  it should "return the original instance instead of an identical copy whenever possible" in all(_.intersectSharingTest())

  "The diff method" should "work identical to the filterNot method" in all(_.diffTest())

  it should "return the original instance instead of an identical copy whenever possible" in all(_.diffTest())

  "The union method" should "produce a set that contains all elements of its left and right argument" in all(_.unionTest())

  it should "return the original instance instead of an identical copy whenever possible" in all(_.unionSharingTest())
}