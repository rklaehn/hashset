import ichi.bench.Thyme

object GetBenchmark extends App {

  val th = Thyme.warmed()
  import th.autoWarmer

  def bench(n:Int) = {
    val reference = scala.collection.immutable.HashSet.empty ++ (0 until n)
    val improved = scala.collection.immutable7.HashSet.empty ++ (0 until n)
    th.pbenchOffWarm(s"get $n")(() => reference(n/2))(() => improved(n/2))

  }

  for(size<-Seq(1,10,100,1000,10000,100000,1000000))
    bench(size)
}
