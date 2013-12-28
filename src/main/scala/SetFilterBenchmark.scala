import scala.collection.immutable._
import scala.collection.immutable2.{HashSet => HashSet2}
import ichi.bench.Thyme

object SetFilterBenchmark extends App {

  val th = Thyme.warmed()

  val e0 = HashSet.empty[Int]
  val e1 = HashSet2.empty[Int]

  def compare(n:Int, k:Int) = {
    val s0 = e0 ++ (0 until n)

    val (_,t0) = th.timePair(s0.filter(_ < k))

    val s1 = e1 ++ (0 until n)
    val (_,t1) = th.timePair(s1.filter(_ < k))

    // th.pbenchOff(s"$n\t$k\t")(s0.filter(_ < k).isEmpty)(s1.filter(_ < k).isEmpty)
    (t0.runTime, t1.runTime)
  }

  def tToS(time:Double) = (time*1000000).formatted("%.3f")

  def rToS(r:Double) = r.formatted("%.3f")

  def benchmark(s:Set[Int]) : Unit = {
    val size = s.size
    var j = 0
    while(true) {
      var i = 1
      while(i<size) {
        s.filter(_<i)
        i*=2
        j+=1
        if(j>1000)
          return
      }
    }
  }

  def bench() = {
    var total0 = 0.0
    var total1 = 0.0
    val sizes = 0 +: (0 until 12).map(x => math.pow(2,x).toInt)
    println(s"n\tk\tt0[us]\tt1[us]\tt0/t1")
    def execute(n:Int, k:Int) {
      val (t0, t1) = compare(n,k)
      val r = t0 / t1
      println(s"$n\t$k\t${tToS(t0)}\t${tToS(t1)}\t${rToS(r)}")
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
  // poor mans warmup
  for(i<-0 until 100)
    bench()

  println("----")
  // this one counts
  bench()

  val s0 = e0 ++ (0 until 100000)
  val s1 = e1 ++ (0 until 100000)
  th.ptime(benchmark(s0))
  th.ptime(benchmark(s1))
  th.ptime(benchmark(s0))
  th.ptime(benchmark(s1))
}
