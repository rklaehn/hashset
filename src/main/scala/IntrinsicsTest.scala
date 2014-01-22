import ichi.bench.Thyme
import java.lang.{Long => JavaLong}
import scala.collection.immutable.LongMap
import scala.collection.SetLike

final class LongSet private (private val root:Any) extends Set[Long] with SetLike[Long, LongSet] {

  def contains(elem: Long): Boolean = Node.contains(root, elem)

  def +(elem: Long): LongSet = copy(Node.insert(root, elem))

  def -(elem: Long): LongSet = ???

  def iterator: Iterator[Long] = {
    val result = Array.newBuilder[Long]
    foreach(result.+=)
    result.result().iterator
  }

  override def foreach[U](f:Long => U) = Node.traverse(root, f)

  override def empty = LongSet.empty

  private[this] def copy(value:Any) = if(value.asInstanceOf[AnyRef] eq root.asInstanceOf[AnyRef]) this else new LongSet(value)
}

object LongSet {

  val empty = new LongSet(null)
}

final class Node(val prefix:Long, val level:Int, val bitmap:Int, val children:Array[Any]) {
  require(JavaLong.bitCount(bitmap) > 1)

  final def mask = Node.mask(level)

  final def prefixBits = ~mask
}

trait Indexable[T] {
  def index(value:T) : Long
}

object Node {
  import Implicits._

  @inline private def mask(level:Int) =
    -1L >>> level

  /*
  @inline def level(a:Long, b:Long) = {
    val lz = JavaLong.numberOfLeadingZeros(a ^ b)
    (lz / 4) * 4
  }

  @inline private[this] def index(value:Long, level:Int) = {
    (value >>> (60 - level)).toInt & 0x1F
  } */

  @inline def level(a:Long, b:Long) = {
    val lz = JavaLong.numberOfLeadingZeros(a ^ b)
    if(lz < 4)
      0
    else
      ((lz + 1) / 5) * 5 - 1
  }

  @inline private[this] def index(value:Long, level:Int) =
    (value >>> (59 - level)).toInt & 0x1F

  def stats(node:Any, hist:Array[Int] = new Array[Int](33)) : IndexedSeq[Int] = node match {
    case null =>
      hist
    case node:Node =>
      hist(Integer.bitCount(node.bitmap)) += 1
      for(child<-node.children)
        stats(child, hist)
      hist
    case _ =>
      val leaf = node.asInstanceOf[Long]
      hist
  }

  def print(node:Any, prefix:String = "") : Unit = node match {
    case null =>
      println(prefix + "empty")
    case node:Node =>
      val prefix1 = prefix + "    "
      println(prefix + "Node(" + node.prefix + "," + node.level + "," + node.bitmap.toBin + "," + node.children.length + ")")
      for(child<-node.children)
        print(child, prefix1)
    case _ =>
      val leaf = node.asInstanceOf[Long]
      println(prefix + leaf)
  }

  def join[T](va:Long, a: Any, vb:Long, b: Any): Node = {
    val level = this.level(va, vb)
    val mask = this.mask(level)
    val prefix = (va & (~mask))
    val ia = index(va, level)
    val ib = index(vb, level)
    val bitmap = (1 << ia) | (1 << ib)
    val children : Array[Any] = if (ia < ib) Array(a, b) else Array(b, a)
    new Node(prefix, level, bitmap, children)
  }

  def contains(node:Any, value:Long) : Boolean = node match {
    case null =>
      false
    case node:Node =>
      if((value & node.prefixBits) != node.prefix)
        false
      else {
        val index = this.index(value, node.level)
        val offset =
          if(node.bitmap == -1) {
            index
          } else {
            val belowMask = (1 << index) - 1
            Integer.bitCount(node.bitmap & belowMask)
          }
        contains(node.children(offset), value)
      }
    case _ =>
      val leaf = node.asInstanceOf[Long]
      leaf == value
  }

  def traverse[U](node:Any, f:Long => U) : Unit = node match {
    case null =>
    case node:Node =>
      var i = 0
      while (i < node.children.length) {
        traverse(node.children(i), f)
        i += 1
      }
    case _ =>
      val leaf = node.asInstanceOf[Long]
      f(leaf)
  }

  def create(values:Seq[Long]) : Any = {
    var tree : Any = null
    for(value <- values) {
      tree = Node.insert(tree, value)
    }
    tree
  }

  def insert(node:Any, value:Long) : Any = node match {
    case null =>
      // empty, so just box value as a leaf node
      value
    case node:Node =>
      // another node
      if((value & node.prefixBits) != node.prefix) {
        // no common prefix. create node above
        join(node.prefix, node, value, value)
      } else {
        // common prefix
        val index = this.index(value, node.level)
        val mask = 1 << index
        val offset =
          if(node.bitmap == -1)
            index
          else
            Integer.bitCount(node.bitmap & (mask-1))
        if((node.bitmap & mask) != 0) {
          // bucket already exists
          val child0 = node.children(offset)
          val child1 = insert(child0, value)
          if(child0.asInstanceOf[AnyRef] eq child1.asInstanceOf[AnyRef])
            node
          else {
            val children1 = node.children.clone()
            children1(offset) = child1
            new Node(node.prefix, node.level, node.bitmap, children1)
          }
        } else {
          // bucket is new
          val children0 = node.children
          val children1 = new Array[Any](node.children.length + 1)
          Array.copy(children0, 0, children1, 0, offset)
          children1(offset) = value
          Array.copy(children0, offset, children1, offset + 1, children0.length - offset)
          val bitmap1 = node.bitmap | mask
          new Node(node.prefix, node.level, bitmap1, children1)

        }
      }
    case _ =>
      val leaf = node.asInstanceOf[Long]
      if(leaf == value)
        node
      else
        join(leaf, node, value, value)
  }
}

object Implicits {

  implicit class StringOps(val x:String) extends AnyVal {
    def prepend(n:Int,t:Char) = if(x.length >= n) x else ("0" * (n-x.length)) + x
  }

  implicit class LongOps(val x:Long) extends AnyVal {
    def toBin = x.toBinaryString.prepend(64,'0')
  }

  implicit class IntOps(val x:Int) extends AnyVal {
    def toBin = x.toBinaryString.prepend(32,'0')
  }
}

object IntrinsicsTest extends App {
  import Implicits._

  for(i<-0 until 64) {
    val x = 1L << i
    println(x.toBin + " " + Node.level(x,0))
  }

  def test0() {

    var tree : Any = null
    val random = new scala.util.Random(0)
    for(i <- 0L until 10000L) {
      val value = random.nextInt(1000000000)
      tree = Node.insert(tree, value)
    }
    // Node.print(tree)
    val stats = Node.stats(tree)
    val avg = stats.zipWithIndex.map { case (i,n) => i * n}.sum.toDouble / stats.sum
    println(stats)
    println(avg)
  }

  def test1() {

    var tree : Any = null
    val random = new scala.util.Random(0)
    for(i <- 0L until 10000L) {
      val value = i * 100000L
      tree = Node.insert(tree, value)
    }
    // Node.print(tree)
    val stats = Node.stats(tree)
    val avg = stats.zipWithIndex.map { case (i,n) => i * n}.sum.toDouble / stats.sum
    println(stats)
    println(avg)
  }

  def test2() {

    var tree : Any = null
    val random = new scala.util.Random(0)
    for(i <- 0L until 10000L) {
      val value = i << 10
      tree = Node.insert(tree, value)
    }
    //Node.print(tree)
    val stats = Node.stats(tree)
    val avg = stats.zipWithIndex.map { case (i,n) => i * n}.sum.toDouble / stats.sum
    println(stats)
    println(avg)
  }
  test0()
  test1()
  test2()

  val t = Node.create((-1L to 1L).map(_ + Long.MaxValue))
  Node.print(t)
  Node.traverse(t, x => println(x - Long.MaxValue))


  def debugLevel(a:Long, b:Long) : Unit = {
    val level1 = Node.level(a,b)
    val mask = -1L >>> level1
    val ia = (a >>> (60 - level1)).toInt
    val ib = (b >>> (60 - level1)).toInt
    println("a       " + a.toBin)
    println("b       " + b.toBin)
    println("a^b     " + (a^b).toBin)
    println("level   " + level1)
    println("mask    " + mask.toBin)
    println("~mask   " + (~mask).toBin)
    println("common  " + (a & (~mask)).toBin)
    println("ia      " + ia.toBin)
    println("ib      " + ib.toBin)

  }

  def bitCount0(): Int = {
    var result = 0
    var i = 1L
    while (i != -1L) {
      result += JavaLong.bitCount(i)
      i |= (i << 1)
    }
    result
  }

  def bitCount1(): Int = {
    var result = 0
    var i = 1L
    while (i != -1L) {
      result += MyLong.bitCount(i)
      i |= (i << 1)
    }
    result
  }

  def trailingZeros0(): Int = {
    var result = 0
    var i = 1L
    while (i != 0L) {
      result += JavaLong.numberOfTrailingZeros(i)
      i <<= 1
    }
    result
  }

  def trailingZeros1(): Int = {
    var result = 0
    var i = 1L
    while (i != 0L) {
      result += MyLong.numberOfTrailingZeros(i)
      i <<= 1
    }
    result
  }

  def makeSet1(elements:Seq[Long]) : Any = {
    Node.create(elements)
  }

  def makeSet0(elements:Seq[(Long,Unit)]) : Any = {
    LongMap.empty[Unit] ++ elements
  }


  debugLevel(0,1)
  debugLevel(0,10)
  debugLevel(0,100000000000L)
  val th = Thyme.warmed()

  import th.autoWarmer

  val l = LongMap.empty[Int]
  val a = (0L until 1000L).toArray
  val b = a.map(x => x -> (())).toArray
  val s1 = LongSet.empty ++ (0L until 1000000L)
  val s0 = scala.collection.immutable.Set.empty[Long] ++ (0L until 1000000L)
  th.pbenchOffWarm("bitCount")(() => bitCount0())(() => bitCount1())
  th.pbenchOffWarm("numberOfTrailingZeros")(() => trailingZeros0())(() => trailingZeros1())
  th.pbenchOffWarm("mkSet")(() => makeSet1(a))(() => makeSet0(b))
  th.pbenchOffWarm("set.contains")(() => s1.contains(500000L))(() => s0.contains(500000L))
}
