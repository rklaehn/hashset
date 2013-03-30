import com.google.caliper._
import scala.collection.immutable._

class FilterBenchmark extends SimpleBenchmark {
  @Param(Array("1", "100", "10000", "1000000")) var size:Int = 0

  var set = Set.empty[Int]

  protected override def setUp() {
    set = HashSet((0 until size):_*)
  }

  def timeFilterTrue(reps:Int) = {
    var i=0
    var result = set
    val max = size
    while(i<reps) {
      result = set.filter(_<max)
      i+=1
    }
    result
  }

  def timeFilterThreeQuarter(reps:Int) = {
    var i=0
    var result = set
    val max = (size*3)/4
    while(i<reps) {
      result = set.filter(_<max)
      i+=1
    }
    result
  }

  def timeFilterHalf(reps:Int) = {
    var i=0
    var result = set
    val max = size/2
    while(i<reps) {
      result = set.filter(_<max)
      i+=1
    }
    result
  }

  def timeFilterQuarter(reps:Int) = {
    var i=0
    var result = set
    val max = size/4
    while(i<reps) {
      result = set.filter(_<max)
      i+=1
    }
    result
  }

  def timeFilterFalse(reps:Int) = {
    var i=0
    var result = set
    val max = 0
    while(i<reps) {
      result = set.filter(_<max)
      i+=1
    }
    result
  }
}

object FilterBenchmark extends App {
  Runner.main(classOf[FilterBenchmark], args)
}
