

object ReplBenchmarks extends App {

  def diffBench() = {
    import scala.collection.immutable.HashSet
    val th = ichi.bench.Thyme.warmed()

    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 10000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      // a and b need to be completely independent sets. If we would create a from b, we would not really test the
      // general case since some nodes would be shared and there would be cutoffs due to eq checks!
      val a = HashSet.empty ++ (0 until i)
      val b = HashSet.empty ++ (0 until i).map(_ + sep)
      th.pbenchOffWarm(s"diff(0..$i,$sep..${sep + i})")(() => a diff b)(() => (a /: b.seq) (_ - _))
    }
  }

  def unionBench() = {
    import scala.collection.immutable.HashSet
    val th = ichi.bench.Thyme.warmed()

    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 10000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      // a and b need to be completely independent sets. If we would create a from b, we would not really test the
      // general case since some nodes would be shared and there would be cutoffs due to eq checks!
      val a = HashSet.empty ++ (0 until i)
      val b = HashSet.empty ++ (0 until i).map(_ + sep)
      th.pbenchOffWarm(s"union(0..$i,$sep..${sep + i})")(() => a union b)(() => (a /: b.seq)(_ + _))
    }
  }

  def intersectBench() = {
    import scala.collection.immutable.HashSet
    val th = ichi.bench.Thyme.warmed()

    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 10000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      // a and b need to be completely independent sets. If we would create a from b, we would not really test the
      // general case since some nodes would be shared and there would be cutoffs due to eq checks!
      val a = HashSet.empty ++ (0 until i)
      val b = HashSet.empty ++ (0 until i).map(_ + sep)
      // this comparison relies on that filter still uses the old builder-based approach
      th.pbenchOffWarm(s"diff(0..$i,$sep..${sep + i})")(() => a intersect b)(() => a filter b)
    }
  }

  import scala.collection.immutable.HashSet

  implicit class HashSetExtensions[T](val value: HashSet[T]) extends AnyVal {

    def builderBasedFilter(p: T => Boolean): HashSet[T] = {
      var b = HashSet.newBuilder[T]
      for (x <- value)
        if (p(x)) b += x
      b.result
    }
  }

  def filterBench() = {
    import scala.collection.immutable.HashSet
    val th = ichi.bench.Thyme.warmed()
    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 100000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      val s = HashSet.empty ++ (0 until i)
      val t = HashSet.empty ++ (0 until i)
      th.pbenchOffWarm(s"filter i=$i sep=$sep")(() => s.filter(_ < sep))(() => t.builderBasedFilter(_ < sep))
    }
  }

  def filterNotOverheadBench() = {
    import scala.collection.immutable.HashSet
    val th = ichi.bench.Thyme.warmed()
    import th.autoWarmer

    for {
      i <- Seq(1, 10, 100, 1000, 10000, 100000)
      o <- Seq(0.0, 0.25, 0.5, 0.75, 1.0)
    } {
      val sep = (i * o).toInt
      val s = HashSet.empty ++ (0 until i)
      val t = HashSet.empty ++ (0 until i)
      th.pbenchOffWarm(s"filter i=$i sep=$sep")(() => s.filter(_ < sep))(() => t.filterNot(_ >= sep))
    }
  }
}
