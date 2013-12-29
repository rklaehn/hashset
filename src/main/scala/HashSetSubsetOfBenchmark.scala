import ichi.bench.Thyme
import scala.collection.immutable2.{HashSet => HashSet2}
import scala.collection.immutable.HashSet


object HashSetSubsetOfBenchmark extends App {

  def timeToString(time:Double) = (time*1000000).formatted("%.3f")

  val th = Thyme.warmed()

  def subsetComparison[T](sizes:Seq[Int], empty0:Set[T], empty1:Set[T], mkKey:Int => T) {

    val empty0 = HashSet.empty
    val o = sizes.max + 1
    for {
      i <- sizes
      k <- sizes
      if i >= k
    }
    {
      val ikeys = (0 to i).map(mkKey)
      val kkeys = (0 to k).map(mkKey)
      val a0 = empty0 ++ ikeys
      val b0 = empty0 ++ kkeys
      val a1 = empty1 ++ ikeys
      val b1 = empty1 ++ kkeys
      // val c = b + o
      // manual warmup
      for(i<-0 until 100) {
        a0.subsetOf(b0)
        a1.subsetOf(b1)
      }
      val (_,b) = th.benchPair(a0.subsetOf(b0))
      println(b.runtime)
      val r = th.pbenchOff(title = "subsetOf")(a1.subsetOf(b1))(a0.subsetOf(b0))
      //val (r,t) = th.timePair(b0.subsetOf(a0))
      //val runtime = timeToString(t.runTime)

      //println(s"$i\t$k\t$runtime")
    }
  }

  val sizes = Seq(0,1,10,100,1000,10000)
  subsetComparison(sizes, HashSet.empty[Int], HashSet2.empty[Int], identity)
}
