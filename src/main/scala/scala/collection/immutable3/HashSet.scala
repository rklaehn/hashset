// This one is (c) Rex Kerr

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable3

import generic._
import scala.collection.immutable.ListSet

/** This class implements immutable sets using a hash trie.
  *
  *  '''Note:''' The builder of this hash set may return specialized representations for small sets.
  *
  *  @tparam A      the type of the elements contained in this hash set.
  *
  *  @author  Martin Odersky
  *  @author  Tiark Rompf
  *  @version 2.8
  *  @since   2.3
  *  @define Coll `immutable.HashSet`
  *  @define coll immutable hash set
  */
@SerialVersionUID(2L)
@deprecatedInheritance("The implementation details of immutable hash sets make inheriting from them unwise.", "2.11.0")
class HashSet[A] extends AbstractSet[A]
with scala.collection.immutable.Set[A]
with GenericSetTemplate[A, HashSet]
with SetLike[A, HashSet[A]]
with Serializable
{
  override def companion: GenericCompanion[HashSet] = HashSet

  //class HashSet[A] extends Set[A] with SetLike[A, HashSet[A]] {

  override def par = ???

  override def size: Int = 0

  override def empty = HashSet.empty[A]

  def iterator: Iterator[A] = Iterator.empty

  override def foreach[U](f: A =>  U): Unit = { }

  def contains(e: A): Boolean = get0(e, computeHash(e), 0)

  override def + (e: A): HashSet[A] = updated0(e, computeHash(e), 0)

  override def + (elem1: A, elem2: A, elems: A*): HashSet[A] =
    this + elem1 + elem2 ++ elems
  // TODO: optimize (might be able to use mutable updates)

  def - (e: A): HashSet[A] =
    removed0(e, computeHash(e), 0)

  protected def elemHashCode(key: A) = key.##

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[collection] def computeHash(key: A) = improve(elemHashCode(key))

  protected def get0(key: A, hash: Int, level: Int): Boolean = false

  def updated0(key: A, hash: Int, level: Int): HashSet[A] =
    new HashSet.HashSet1(key, hash)

  protected def removed0(key: A, hash: Int, level: Int): HashSet[A] = this

  override def filter(p: A => Boolean): HashSet[A] = {
    val ans = filter0(false, p)
    if (ans eq null) HashSet.empty[A]
    else ans
  }

  override def filterNot(p: A => Boolean): HashSet[A] = {
    val ans = filter0(true, p)
    if (ans eq null) HashSet.empty[A]
    else ans
  }

  protected def filter0(toss: Boolean, p: A => Boolean): HashSet[A] = null

  protected def writeReplace(): AnyRef = new HashSet.SerializationProxy(this)

}

/** $factoryInfo
  *  @define Coll `immutable.HashSet`
  *  @define coll immutable hash set
  *
  *  @author  Tiark Rompf
  *  @since   2.3
  *  @define Coll `immutable.HashSet`
  *  @define coll immutable hash set
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
object HashSet extends ImmutableSetFactory[HashSet] {

  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: HashSet[A] = EmptyHashSet.asInstanceOf[HashSet[A]]

  private object EmptyHashSet extends HashSet[Any] { }

  // utility method to create a HashTrieSet from two leaf HashSets (HashSet1 or HashSetCollision1) with non-colliding hash code)
  private def makeHashTrieSet[A](hash0:Int, elem0:HashSet[A], hash1:Int, elem1:HashSet[A], level:Int) : HashTrieSet[A] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if(index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashSet[A]](2)
      if(index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieSet[A](bitmap, elems, elem0.size + elem1.size)
    } else {
      val elems = new Array[HashSet[A]](1)
      val bitmap = (1 << index0)
      val child = makeHashTrieSet(hash0, elem0, hash1, elem1, level + 5)
      elems(0) = child
      new HashTrieSet[A](bitmap, elems, child.size)
    }
  }

  // TODO: add HashSet2, HashSet3, ...

  class HashSet1[A](private[HashSet] val key: A, private[HashSet] val hash: Int) extends HashSet[A] {
    override def size = 1

    override def get0(key: A, hash: Int, level: Int): Boolean =
      (hash == this.hash && key == this.key)

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) this
      else {
        if (hash != this.hash) {
          makeHashTrieSet(this.hash, this, hash, new HashSet1(key, hash), level)
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashSetCollision1(hash, ListSet.empty + this.key + key)
        }
      }

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) HashSet.empty[A] else this

    override def filter0(toss: Boolean, p: A => Boolean) =
      if (!(toss ^ p(key))) null else this

    override def iterator: Iterator[A] = Iterator(key)
    override def foreach[U](f: A => U): Unit = f(key)
  }

  private[immutable3] class HashSetCollision1[A](private[HashSet] val hash: Int, val ks: ListSet[A])
    extends HashSet[A] {

    override def size = ks.size

    override def get0(key: A, hash: Int, level: Int): Boolean =
      if (hash == this.hash) ks.contains(key) else false

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) new HashSetCollision1(hash, ks + key)
      else makeHashTrieSet(this.hash, this, hash, new HashSet1(key, hash), level)

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) {
        val ks1 = ks - key
        if(ks1.isEmpty)
          HashSet.empty[A]
        else if(ks1.tail.isEmpty)
          new HashSet1(ks1.head, hash)
        else
          new HashSetCollision1(hash, ks1)
      } else this

    override def filter0(toss: Boolean, p: A => Boolean) = {
      val ks1 = if (toss) ks.filterNot(p) else ks.filter(p)
      ks1.size match {
        case 0 =>
          null
        case 1 =>
          new HashSet1(ks1.head, hash)
        case x if x == ks.size =>
          this
        case _ =>
          new HashSetCollision1(hash, ks1)
      }
    }

    override def iterator: Iterator[A] = ks.iterator
    override def foreach[U](f: A => U): Unit = ks.foreach(f)

    private def writeObject(out: java.io.ObjectOutputStream) {
      // this cannot work - reading things in might produce different
      // hash codes and remove the collision. however this is never called
      // because no references to this class are ever handed out to client code
      // and HashTrieSet serialization takes care of the situation
      sys.error("cannot serialize an immutable.HashSet where all items have the same 32-bit hash code")
      //out.writeObject(kvs)
    }

    private def readObject(in: java.io.ObjectInputStream) {
      sys.error("cannot deserialize an immutable.HashSet where all items have the same 32-bit hash code")
      //kvs = in.readObject().asInstanceOf[ListSet[A]]
      //hash = computeHash(kvs.)
    }

  }

  class HashTrieSet[A](private val bitmap: Int, private[collection] val elems: Array[HashSet[A]], private val size0: Int)
    extends HashSet[A] {
    assert(Integer.bitCount(bitmap) == elems.length)
    // assertion has to remain disabled until SI-6197 is solved
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))

    override def size = size0

    override def get0(key: A, hash: Int, level: Int): Boolean = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == - 1) {
        elems(index & 0x1f).get0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask-1))
        // TODO: might be worth checking if sub is HashTrieSet (-> monomorphic call site)
        elems(offset).get0(key, hash, level + 5)
      } else
        false
    }

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        // TODO: might be worth checking if sub is HashTrieSet (-> monomorphic call site)
        val sub = elems(offset)
        val subNew = sub.updated0(key, hash, level + 5)
        if (sub eq subNew) this
        else {
          val elemsNew = new Array[HashSet[A]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          new HashTrieSet(bitmap, elemsNew, size + (subNew.size - sub.size))
        }
      } else {
        val elemsNew = new Array[HashSet[A]](elems.length + 1)
        Array.copy(elems, 0, elemsNew, 0, offset)
        elemsNew(offset) = new HashSet1(key, hash)
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieSet(bitmapNew, elemsNew, size + 1)
      }
    }

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask-1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        // TODO: might be worth checking if sub is HashTrieMap (-> monomorphic call site)
        val subNew = sub.removed0(key, hash, level + 5)
        if (sub eq subNew) this
        else if (subNew.isEmpty) {
          val bitmapNew = bitmap ^ mask
          if (bitmapNew != 0) {
            val elemsNew = new Array[HashSet[A]](elems.length - 1)
            Array.copy(elems, 0, elemsNew, 0, offset)
            Array.copy(elems, offset + 1, elemsNew, offset, elems.length - offset - 1)
            val sizeNew = size - sub.size
            // if we have only one child, which is not a HashTrieSet but a self-contained set like
            // HashSet1 or HashSetCollision1, return the child instead
            if (elemsNew.length == 1 && !elemsNew(0).isInstanceOf[HashTrieSet[_]])
              elemsNew(0)
            else
              new HashTrieSet(bitmapNew, elemsNew, sizeNew)
          } else
            HashSet.empty[A]
        } else {
          val elemsNew = new Array[HashSet[A]](elems.length)
          Array.copy(elems, 0, elemsNew, 0, elems.length)
          elemsNew(offset) = subNew
          val sizeNew = size + (subNew.size - sub.size)
          new HashTrieSet(bitmap, elemsNew, sizeNew)
        }
      } else {
        this
      }
    }

    override def filter0(toss: Boolean, p: A => Boolean): HashSet[A] = {
      // First check if everything is kept
      var i = 0
      var first = elems(0).filter0(toss, p)
      while ((first eq elems(i)) && i+1 < elems.length) {
        i += 1
        first = elems(i).filter0(toss, p)
      }
      if (i+1 == elems.length && (first eq elems(i))) return this

      // Then check if everything or all but one is rejected
      val nSame = i
      var iFirst = if (first eq null) -1 else nSame
      var second = null: HashSet[A]
      while ((second eq null) && i+1 < elems.length) {
        i += 1
        second = elems(i).filter0(toss, p)
      }
      val iSecond = if (second eq null) -1 else i
      while (iFirst < 0 && i+1 < elems.length) {
        i += 1
        first = elems(i).filter0(toss, p)
        if (first ne null) iFirst = i
      }
      if (i+1 == elems.length) {
        def wrapIfNeeded(h: HashSet[A], n: Int) = {
          if (h.isInstanceOf[HashTrieSet[_]]) {
            var j = 0
            var flag = 1
            while ((bitmap & flag) != flag) flag <<= 1
            while (j < n) {
              flag <<= 1
              while ((bitmap & flag) != flag) flag <<= 1
              j += 1
            }
            new HashTrieSet(flag, Array[HashSet[A]](h), h.size)
          }
          else h
        }
        if (nSame==0) {
          if (iFirst < 0) return wrapIfNeeded(second, iSecond)
          else if (iSecond < 0) return wrapIfNeeded(first, iFirst)
        }
        else if (nSame==1 && iFirst<0 && iSecond<0) return wrapIfNeeded(elems(0), 0)
      }

      // Now we know we have at least two items; pack them.
      // Flags first
      var found = nSame + (if (iFirst<0) 0 else 1) + (if (iSecond<0) 0 else 1)
      var flag = 1
      var j = 0
      while (j < nSame) {
        if ((bitmap & flag) == flag) j += 1
        flag <<= 1
      }
      var bitmapNew = bitmap & (flag-1)
      while (j <= i) {
        if ((bitmap & flag) == flag) {
          if (j==iFirst || j==iSecond) bitmapNew |= flag
          j += 1
        }
        flag <<= 1
      }
      // Now data
      var elemsNew = java.util.Arrays.copyOf(elems, elems.length - (i - found))
      if (iFirst >= 0) {
        if (iSecond >= 0) {
          val fls = if (iFirst < iSecond) 0 else 1
          elemsNew(nSame+fls) = first
          elemsNew(nSame+(1-fls)) = second
        }
        else elemsNew(nSame) = first
      }
      else if (iSecond >= 0) elemsNew(nSame) = second
      // Now scan the rest
      j = found
      while (i+1 < elems.length) {
        i += 1
        while ((bitmap & flag) != flag) flag <<= 1
        val temp = elems(i).filter0(toss, p)
        if (temp ne null) {
          bitmapNew |= flag
          elemsNew(j) = temp
          j += 1
        }
        flag <<= 1
      }
      // Pack data and find size
      if (elemsNew.length > j) elemsNew = java.util.Arrays.copyOf(elemsNew, j)
      var sizeNew = 0
      i = 0
      while (i < elemsNew.length) {
        sizeNew += elemsNew(i).size
        i += 1
      }
      new HashTrieSet(bitmapNew, elemsNew, sizeNew)
    }

    override def iterator = ???
    /*

    def time(block: =>Unit) = { val t0 = System.nanoTime; block; println("elapsed: " + (System.nanoTime - t0)/1000000.0) }
    var mOld = OldHashSet.empty[Int]
    var mNew = HashSet.empty[Int]
    time { for (i <- 0 until 100000) mOld = mOld + i }
    time { for (i <- 0 until 100000) mOld = mOld + i }
    time { for (i <- 0 until 100000) mOld = mOld + i }
    time { for (i <- 0 until 100000) mNew = mNew + i }
    time { for (i <- 0 until 100000) mNew = mNew + i }
    time { for (i <- 0 until 100000) mNew = mNew + i }
    time { mOld.iterator.foreach( p => ()) }
    time { mOld.iterator.foreach( p => ()) }
    time { mOld.iterator.foreach( p => ()) }
    time { mNew.iterator.foreach( p => ()) }
    time { mNew.iterator.foreach( p => ()) }
    time { mNew.iterator.foreach( p => ()) }

    */
    override def foreach[U](f: A =>  U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }
  }

  @SerialVersionUID(2L) private class SerializationProxy[A,B](@transient private var orig: HashSet[A]) extends Serializable {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for (e <- orig) {
        out.writeObject(e)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val e = in.readObject().asInstanceOf[A]
        orig = orig + e
      }
    }

    private def readResolve(): AnyRef = orig
  }

}

