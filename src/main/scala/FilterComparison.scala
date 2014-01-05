object FilterComparison extends App {

  val th = ichi.bench.Thyme.warmed()

  // filterNot with !p(_)
  val s1 = scala.collection.immutable5.HashSet.empty[Int]

  // filterNot with negate flag
  val s2 = scala.collection.immutable6.HashSet.empty[Int]

  def filterBench() = {
    val th = ichi.bench.Thyme.warmed()
    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 100000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      val s = s1 ++ (0 until i)
      val t = s2 ++ (0 until i)
      th.pbenchOffWarm(s"filter i=$i sep=$sep")(() => s.filter(_ < sep).size)(() => t.filter(_ < sep).size)
    }
  }

  def filterNotBench() = {
    val th = ichi.bench.Thyme.warmed()
    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 100000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      val s = s1 ++ (0 until i)
      val t = s2 ++ (0 until i)
      th.pbenchOffWarm(s"filterNot i=$i sep=$sep")(() => s.filterNot(_ < sep).size)(() => t.filterNot(_ < sep).size)
    }
  }

  filterBench()
  filterNotBench()

}
