import scala.collection.immutable2.HashSet

object DepthTest extends App {

  private[this] final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  def keyForCode(code:Int) : Int = {
    var i = Int.MinValue
    while(i<=Int.MaxValue) {
      if(improve(i) == code)
        return i
      i+=1
    }
    throw new Exception("improve not bijective")
  }

  val kfcs =
    for(i <- 0 until 4)
    yield keyForCode(i << 30)

  val hs = HashSet.empty ++ kfcs
  for(e <- hs.iterator)
    println(e)
  HashSet.printStructure(hs, "")
  println(hs)
}
