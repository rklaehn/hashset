import scala.collection.immutable._
import scala.collection.immutable2.{HashSet => HashSet2}
import scala.collection.immutable3.{HashSet => HashSet3}
import ichi.bench.Thyme

object SetFilterBenchmark extends App {

  val th = Thyme.warmed()
  import th.autoWarmer

  val e0 = HashSet.empty[Int] // scala
  val e1 = HashSet2.empty[Int] // me
  val e2 = HashSet3.empty[Int] // rex

  def compare2(n:Int, k:Int) = {
    val s0 = e0 ++ (0 until n)
    val (r0,t0) = th.benchPairWarm(() => s0.filter(_ < k))

    val s1 = e1 ++ (0 until n)
    val (r1,t1) = th.benchPairWarm(() => s1.filter(_ < k))

    require(r0 == r1)

    // th.pbenchOff(s"$n\t$k\t")(s0.filter(_ < k).isEmpty)(s1.filter(_ < k).isEmpty)
    (t0.runtime, t1.runtime)
  }
  
  def compare3(n:Int, k:Int) = {
    val s0 = e0 ++ (0 until n)
    val (r0,t0) = th.benchPairWarm(() => s0.filter(_ < k))

    val s1 = e1 ++ (0 until n)
    val (r1,t1) = th.benchPairWarm(() => s1.filter(_ < k))

    val s2 = e2 ++ (0 until n)
    val (r2,t2) = th.benchPairWarm(() => s2.filter(_ < k))

    require(r0 == r1 && r1 == r2)

    // th.pbenchOff(s"$n\t$k\t")(s0.filter(_ < k).isEmpty)(s1.filter(_ < k).isEmpty)
    (t0.runtime, t1.runtime, t2.runtime)
  }

  def tToS(time:Double) = (time*1000000).formatted("%.3f")

  def rToS(r:Double) = r.formatted("%.3f")

  def linearBenchmark(n:Int) {

    def work(s:Set[Int]) : Int = {
      var result = 0
      for(i<-0 until s.size by s.size / 100) {
        result += s.filter(_<i).size
      }
      result
    }

    val a = HashSet.empty[Int] ++ (0 until n)
    val b = HashSet2.empty[Int] ++ (0 until n)
    val (r0, t0) = th.benchPairWarm(() => work(a))
    val (r1, t1) = th.benchPairWarm(() => work(b))

    val time0 = tToS(t0.runtime)
    val time1 = tToS(t1.runtime)
    val ratio = rToS(t0.runtime/t1.runtime)
    println(s"$time0\t$time1\t$ratio")
  }
  linearBenchmark(10000)

  def bench2() = {
    var total0 = 0.0
    var total1 = 0.0
    val sizes = 0 +: (0 until 12).map(x => math.pow(2,x).toInt)
    println(s"n\tk\tt0[us]\tt1[us]\tt0/t1")
    def execute(n:Int, k:Int) {
      val (t0, t1) = compare2(n,k)
      val r1 = t0 / t1
      println(s"$n\t$k\t${tToS(t0)}\t${tToS(t1)}\t${rToS(r1)}")
      total0 += t0
      total1 += t1
    }
    for {
      n <- sizes
      k <- sizes
      if k <= n
    } execute(n, k)
    println("Old set total [us]" + tToS(total0))
    println("New set total [us]" + tToS(total1))
  }

  def bench3() = {
    var total0 = 0.0
    var total1 = 0.0
    var total2 = 0.0
    val sizes = 0 +: (0 until 12).map(x => math.pow(2,x).toInt)
    println(s"n\tk\tt0[us]\tt1[us]\tt2[us]\tt0/t1\tt0/t2")
    def execute(n:Int, k:Int) {
      val (t0, t1, t2) = compare3(n,k)
      val r1 = t0 / t1
      val r2 = t0 / t2
      println(s"$n\t$k\t${tToS(t0)}\t${tToS(t1)}\t${tToS(t2)}\t${rToS(r1)}\t${rToS(r2)}")
      total0 += t0
      total1 += t1
      total2 += t2
    }
    for {
      n <- sizes
      k <- sizes
      if k <= n
    } execute(n, k)
    println("Old set total [us]" + tToS(total0))
    println("New set (me) total [us]" + tToS(total1))
    println("New set (Rex) total [us]" + tToS(total2))
  }

  bench2()

  def biasedToFalseBenchmark(s:Set[Int]) : Unit = {
    val size = s.size
    var j = 0
    var k = 0L
    val total = 100000000
    while(true) {
      var i = 1
      while(i<size) {
        s.filter(_<i)
        i *= 2
        j += i
        k += size
        if(j>total)
          return
      }
    }
  }

  def biased() {
    val s0 = e0 ++ (0 until 10000)
    val s1 = e1 ++ (0 until 10000)
    th.ptime(biasedToFalseBenchmark(s0))
    th.ptime(biasedToFalseBenchmark(s1))
    th.ptime(biasedToFalseBenchmark(s0))
    th.ptime(biasedToFalseBenchmark(s1))
  }
  biased()

  def sBenchmark(s:Set[Int],k:Int) {
    var j = 0
    while(true) {
      j += s.size
      s.filter(_<k)
      if(j>100000000)
        return
    }
  }

  val s0 = e0 ++ (0 until 2048)
  val s1 = e1 ++ (0 until 2048)
  th.ptime(sBenchmark(s0,8))
  th.ptime(sBenchmark(s1,8))
  th.ptime(sBenchmark(s0,8))
  th.ptime(sBenchmark(s1,8))
  th.ptime(sBenchmark(s0,8))
  th.ptime(sBenchmark(s1,8))
  th.ptime(sBenchmark(s0,8))
  th.ptime(sBenchmark(s1,8))
}
