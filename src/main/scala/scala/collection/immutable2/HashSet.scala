package scala.collection.immutable2

import collection.immutable.ListSet
import collection.{GenTraversableOnce, GenSet, SetLike}
import collection.generic.{CanBuildFrom, ImmutableSetFactory}

/**
 * An efficient immutable set
 * @tparam A the element type. invariant, unfortunately
 */
sealed abstract class HashSet[A] extends Set[A] with SetLike[A, HashSet[A]] {

  import HashSet.{nullToEmpty, LeafHashSet, HashSet1, FilterState}

  override def contains(e: A): Boolean = contains0(e, computeHash(e), 0)

  override def +(e: A): HashSet[A] = nullToEmpty(union0(new HashSet1[A](e, computeHash(e)), 0))

  override def -(e: A): HashSet[A] = nullToEmpty(removed0(e, computeHash(e), 0))

  override def empty = HashSet.empty[A]

  final override def ++(that: GenTraversableOnce[A]) = that match {
    case that: HashSet[A] => this ++ that
    case _ => super.++(that)
  }

  final def ++(that: HashSet[A]) = union(that)

  final override def union(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] => union(that)
    case _ => super.union(that)
  }

  final def union(that: HashSet[A]) = nullToEmpty(union0(that, new BufferPool[A]))

  final override def intersect(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] => this.intersect(that)
    case _ => super.intersect(that)
  }

  final def intersect(that: HashSet[A]): HashSet[A] = nullToEmpty(intersect0(that, new BufferPool[A]))

  final override def diff(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] => this.diff(that)
    case _ => super.diff(that)
  }

  final def delta(that:HashSet[A]) : (HashSet[A], HashSet[A]) = (this diff that, that diff this)

  final def diff(that: HashSet[A]): HashSet[A] = nullToEmpty(diff0(that, new BufferPool[A]))

  final override def subsetOf(that: GenSet[A]): Boolean = that match {
    case that: HashSet[A] => this.subsetOf(that)
    case _ => super.subsetOf(that)
  }

  final def subsetOf(that: HashSet[A]): Boolean = subsetOf0(that, 0)

  /*
  override final def filter(p: (A) => Boolean): HashSet[A] = {
    var result = empty
    foreachLeaf {
      case x:HashSet1[A] =>
        if(p(x.key))
          result = result.addLeaf(x)
      case x:HashSetCollision1[A] =>
        val ks1 = x.ks.filter(p)
        result = ks1.size match {
          case 0 => result
          case 1 => result.addLeaf(HashSet1(ks1.head, x.hash))
          case _ => result.addLeaf(x.copy(ks = ks1))
        }
    }
    result
  }
  */

  override def filter(p: (A) => Boolean): HashSet[A] = nullToEmpty(filter0(new FilterState[A](p, size)))

  // override def filter(p: (A) => Boolean): HashSet[A] = new HashSet.FilterOp(p).filter(this)

  override def iterator: Iterator[A]

  override def size: Int

  override def foreach[U](f: A => U): Unit

  def contains0(key: A, hash: Int, level: Int): Boolean

  protected def subsetOf0(that: HashSet[A], level: Int): Boolean

  protected def union0(that: LeafHashSet[A], level: Int): HashSet[A]

  private[HashSet] final def addLeaf(that: LeafHashSet[A]) : HashSet[A] = union0(that, 0)

  protected def foreachLeaf[U](f:LeafHashSet[A] => U) : Unit

  protected def removed0(key: A, hash: Int, level: Int): HashSet[A]

  protected def filter0(p:FilterState[A]): HashSet[A]

  protected def union0(that: HashSet[A], pool: BufferPool[A]): HashSet[A]

  protected def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A]

  protected def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A]

  /**
   * Do not change this without changing the improve method on HMap in the same way!
   */
  private[this] final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  private[this] final def computeHash(key: A) = improve(key.##)
}

object HashSet extends ImmutableSetFactory[HashSet] {

  def printStructure[T](s:HashSet[T],prefix:String) : Unit = {
    s match {
      case x:EmptySet[T] => println(prefix + "EmptySet")
      case x:HashSet1[T] => println(prefix + "HashSet1(" + x.key + ")")
      case x:HashSetCollision1[T] => println(prefix + "HashSetCollision1(" + x.ks.mkString(",") + ")")
      case x:HashTrieSet[T] =>
        println(prefix + "HashTrieSet(" + x.elems.size + ")")
        for(c<-x.elems)
          printStructure(c, prefix + "    ")
    }
  }

  def validate[T](s: HashSet[T], level: Int = 0, mask: Int = 0, value: Int = 0) {
    s match {
      case s: HashSet1[_] =>
        require((s.hash & mask) == value)
      case s: HashSetCollision1[_] =>
        require((s.hash & mask) == value)
      case s: HashTrieSet[_] =>
        var i = 0
        var j = 0
        var bit = 1
        val bitmap = s.bitmap
        val elems = s.elems
        require(Integer.bitCount(bitmap) == elems.length)
        require(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))
        while (bit != 0) {
          if ((bitmap & bit) == bit) {
            validate(elems(i), level + 5, mask | (0x1f << level), value | (j << level))
            i += 1
          }
          j += 1
          bit <<= 1
        }
      case e =>
        require(level == 0)
    }
  }

  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: HashSet[A] = emptySet.asInstanceOf[HashSet[A]]

  private val emptySet = new EmptySet[Nothing]

  private def makeHashTrieSet[A](hash0: Int, elem0: HashSet[A], hash1: Int, elem1: HashSet[A], level: Int, size: Int): HashTrieSet[A] = {
    val index0 = (hash0 >>> level) & 0x1f
    val index1 = (hash1 >>> level) & 0x1f
    if (index0 != index1) {
      val bitmap = (1 << index0) | (1 << index1)
      val elems = new Array[HashSet[A]](2)
      if (index0 < index1) {
        elems(0) = elem0
        elems(1) = elem1
      } else {
        elems(0) = elem1
        elems(1) = elem0
      }
      new HashTrieSet[A](bitmap, elems, size)
    } else {
      val elems = new Array[HashSet[A]](1)
      val bitmap = (1 << index0)
      val child = makeHashTrieSet(hash0, elem0, hash1, elem1, level + 5, size)
      elems(0) = child
      new HashTrieSet[A](bitmap, elems, size)
    }
  }

  @inline private def nullToEmpty[A](s: HashSet[A]): HashSet[A] = if (s eq null) empty[A] else s

  private final class EmptySet[A] extends HashSet[A] {

    override def size = 0

    override def iterator: Iterator[A] = Iterator.empty

    override def foreach[U](f: A => U) {}

    def contains0(key: A, hash: Int, level: Int): Boolean = false

    def subsetOf0(that: HashSet[A], level: Int): Boolean = true

    def union0(that: LeafHashSet[A], level: Int): HashSet[A] = that

    def removed0(key: A, hash: Int, level: Int): HashSet[A] = null

    def filter0(p: FilterState[A]): HashSet[A] = null

    def union0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = that

    def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = null

    def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = null

    def foreachLeaf[U](f: (LeafHashSet[A]) => U): Unit = {}
  }

  sealed abstract class LeafHashSet[A] extends HashSet[A] {
    def hash: Int
  }

  final case class HashSet1[A](key: A, hash: Int) extends LeafHashSet[A] {

    override def hashCode = hash

    override def size = 1

    override def iterator: Iterator[A] = Iterator(key)

    override def foreach[U](f: A => U) {
      f(key)
    }

    def contains0(key: A, hash: Int, level: Int): Boolean =
      (hash == this.hash && key == this.key)

    def subsetOf0(that: HashSet[A], level: Int): Boolean =
      that.contains0(key, hash, level)

    def union0(that: LeafHashSet[A], level: Int): HashSet[A] =
      if (that.hash != this.hash)
        makeHashTrieSet(this.hash, this, that.hash, that, level, 1 + that.size)
      else that match {
        case that: HashSet1[A] =>
          if (this.key == that.key) {
            this
          } else {
            // 32-bit hash collision (rare, but not impossible)
            new HashSetCollision1[A](hash, ListSet.empty + this.key + that.key)
          }
        case that: HashSetCollision1[A] =>
          val ks1 = that.ks + key
          if (ks1.size == that.ks.size)
            that
          else
            new HashSetCollision1[A](hash, ks1)
      }

    def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) null else this

    def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] =
      if (that.contains0(key, hash, pool.level)) this else null

    def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] =
      if (that.contains0(key, hash, pool.level)) null else this

    def filter0(p: FilterState[A]): HashSet[A] =
      if (p(key)) this else null

    def union0(that: HashSet[A], pool: BufferPool[A]) =
      that.union0(this, pool.level)

    def foreachLeaf[U](f: (LeafHashSet[A]) => U): Unit = { f(this) }
  }

  final case class HashSetCollision1[A](hash: Int, ks: ListSet[A]) extends LeafHashSet[A] {
    assert(ks.size > 1)

    override def hashCode = hash * 41 + ks.size

    override def size = ks.size

    override def iterator: Iterator[A] = ks.iterator

    override def foreach[U](f: A => U) {
      ks.foreach(f)
    }

    def contains0(key: A, hash: Int, level: Int): Boolean =
      if (hash == this.hash) ks.contains(key) else false

    def subsetOf0(that: HashSet[A], level: Int): Boolean =
      ks.forall(k => that.contains0(k, hash, level))

    def union0(that: LeafHashSet[A], level: Int): HashSet[A] =
      if (that.hash != this.hash)
        makeHashTrieSet(this.hash, this, that.hash, that, level, ks.size + that.size)
      else that match {
        case that: HashSet1[A] =>
          val ks1 = ks + that.key
          if (ks1.size == ks.size)
            this
          else
            new HashSetCollision1[A](hash, ks1)
        case that: HashSetCollision1[A] =>
          val ks1 = this.ks ++ that.ks
          if (ks1.size == this.ks.size)
            this
          else if (ks1.size == that.ks.size)
            that
          else
            new HashSetCollision1[A](hash, ks1)
      }

    def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) {
        val ks1 = ks - key
        if (ks1.isEmpty)
          null
        else if (ks1.tail.isEmpty)
          new HashSet1(ks1.head, hash)
        else if (ks1.size == this.size)
          this
        else
          new HashSetCollision1(hash, ks1)
      } else this

    def filter0(p: FilterState[A]): HashSet[A] = {
      val ks1 = ks.filter(p)
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

    def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = {
      val ks1 = ks.filter(k => that.contains0(k, hash, pool.level))
      if (ks1.isEmpty)
        null
      else if (ks1.size == that.size)
        that
      else if (ks1.tail.isEmpty)
        new HashSet1(ks1.head, hash)
      else if (ks1.size == this.size)
        this
      else
        new HashSetCollision1(hash, ks1)
    }

    def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = {
      val ks1 = ks.filterNot(k => that.contains0(k, hash, pool.level))
      if (ks1.isEmpty)
        null
      else if (ks1.tail.isEmpty)
        new HashSet1(ks1.head, hash)
      else if (ks1.size == this.size)
        this
      else
        new HashSetCollision1(hash, ks1)
    }

    override def union0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = that match {
      case that: LeafHashSet[A] => this.union0(that, pool.level)
      case that: HashTrieSet[A] => that.union0(this, pool.level)
      case _ => this
    }

    def foreachLeaf[U](f: (LeafHashSet[A]) => U): Unit = { f(this) }
  }

  final case class HashTrieSet[A](bitmap: Int, elems: Array[HashSet[A]], size0: Int) extends HashSet[A] {

    // assert(size0 == elems.map(_.size).sum)
    // assert(Integer.bitCount(bitmap) == elems.length)
    // assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))
    // assert(size0 > 1)

    private[this] def allEq[A <: AnyRef](buffer: Array[A], proto: Array[A]): Boolean = {
      var i = 0
      while (i < proto.length) {
        if (buffer(i) ne proto(i))
          return false
        i += 1
      }
      true
    }

    private[this] def newInstance(bitmap: Int, buffer: Array[HashSet[A]], length: Int, size: Int) = {
      if (length == 0)
        null // marker for the empty set
      else if (length == 1 && !buffer(0).isInstanceOf[HashSet.HashTrieSet[_]])
        buffer(0) // only return HashTrieSet if it is necessary to retain the structure
      else
        new HashTrieSet[A](bitmap, java.util.Arrays.copyOf(buffer, length), size)
    }

    override def size = size0

    override def iterator = elems.iterator.flatMap(_.iterator) // todo: write or borrow an efficient iterator

    override def foreach[U](f: A => U) {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }

    def contains0(key: A, hash: Int, level: Int): Boolean = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      if (bitmap == -1) {
        elems(index & 0x1f).contains0(key, hash, level + 5)
      } else if ((bitmap & mask) != 0) {
        val offset = Integer.bitCount(bitmap & (mask - 1))
        elems(offset).contains0(key, hash, level + 5)
      } else
        false
    }

    def union0(that: LeafHashSet[A], level: Int): HashSet[A] = {
      val index = (that.hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.union0(that, level + 5)
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
        elemsNew(offset) = that
        Array.copy(elems, offset, elemsNew, offset + 1, elems.length - offset)
        val bitmapNew = bitmap | mask
        new HashTrieSet(bitmapNew, elemsNew, size + that.size)
      }
    }

    def removed0(key: A, hash: Int, level: Int): HashSet[A] = {
      val index = (hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val subNew = sub.removed0(key, hash, level + 5)
        if (sub eq subNew) this
        else if (subNew eq null) {
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
            null
        } else {
          if (elems.length == 1 && !subNew.isInstanceOf[HashTrieSet[_]])
            subNew
          else {
            val elemsNew = new Array[HashSet[A]](elems.length)
            Array.copy(elems, 0, elemsNew, 0, elems.length)
            elemsNew(offset) = subNew
            val sizeNew = size + (subNew.size - sub.size)
            new HashTrieSet(bitmap, elemsNew, sizeNew)
          }
        }
      } else {
        this
      }
    }

    def union0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = if (that eq this) this
    else that match {
      case that: HashSet1[A] =>
        this.union0(that, pool.level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        val r = pool.getBuffer()
        var ri = 0
        var rs = 0

        // loop as long as there are bits left in either abm or bbm
        while((abm|bbm)!=0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).union0(b(bi), pool)
            rs += sub1.size
            r(ri) = sub1; ri += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            val sub1 = a(ai)
            rs += sub1.size
            r(ri) = sub1; ri += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            val sub1 = b(bi)
            rs += sub1.size
            r(ri) = sub1; ri += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          }
        }
        pool.freeBuffer()
        val rbm = this.bitmap | that.bitmap
        if (rbm == this.bitmap && allEq(r, this.elems))
          // if the result would be identical to this, we might as well return this
          this
        else if (rbm == that.bitmap && allEq(r, that.elems))
          // if the result would be identical to that, we might as well return that
          that
        else
          newInstance(this.bitmap | that.bitmap, r, ri, rs)
      case hs: HashSetCollision1[A] => that.union0(this, pool)
      case _ => this
    }

    def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = if (that eq this) this
    else that match {
      case that: HashSet1[A] =>
        that.intersect0(this, pool)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // if the bitmasks do not overlap, the result is definitely empty so we can abort here
        if((abm&bbm) == 0)
          return null

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        val r = pool.getBuffer()
        var ri = 0
        var rs = 0
        var rbm = 0

        // loop as long as there are bits left that are set in both abm and bbm
        while((abm&bbm)!=0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).intersect0(b(bi), pool)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              r(ri) = sub1; ri += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          }
        }
        pool.freeBuffer()

        if(rs == size0)
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        else if(rs == that.size0)
          // if the result has the same number of elements as that, it must be identical to that,
          // so we might as well return that
          that
        else
          newInstance(rbm, r, ri, rs)
      case hs: HashSetCollision1[_] => that.intersect0(this, pool)
      case _ => null
    }

    def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = if (that eq this) null
    else that match {
      case hs: HashSet1[A] =>
        removed0(hs.key, hs.hash, pool.level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        val r = pool.getBuffer()
        var ri = 0
        var rs = 0
        var rbm = 0

        // loop until there are no more bits in abm
        while(abm!=0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).diff0(b(bi), pool)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              r(ri) = sub1; ri += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            val sub1 = a(ai)
            rs += sub1.size
            rbm |= alsb
            r(ri) = sub1; ri += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          }
        }
        pool.freeBuffer()
        if(rs == this.size0)
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        else
          newInstance(rbm, r, ri, rs)
      case hs: HashSetCollision1[A] =>
        var this1: HashSet[A] = this
        for (k <- hs.ks)
          if (this1 != null)
            this1 = this1.removed0(k, hs.hash, pool.level)
        this1
      case _ => this
    }

    // unsigned comparison
    @inline private[this] def unsignedCompare(i: Int, j: Int) =
      (i < j) ^ (i < 0) ^ (j < 0)

    def filter0(p: FilterState[A]): HashSet[A] = {
      // store initial offset
      val offset0 = p.offset
      // result size
      var rs = 0
      // bitmap for kept elems
      var kept = 0
      // loop over all elements
      var i = 0
      while (i < elems.length) {
        val result = elems(i).filter0(p)
        if (result ne null) {
          // add the result to the buffer in p
          p.add(result)
          // add the result size
          rs += result.size
          // mark the bit i as kept
          kept |= (1 << i)
        }
        i += 1
      }
      if (p.offset == offset0) {
        // no need to reset the offset in p because it is unchanged
        null
      } else if (rs == size0) {
        // we have to manually reset the offset in p
        p.offset = offset0
        this
      } else if (p.offset == offset0 + 1 && !p.buffer(offset0).isInstanceOf[HashTrieSet[A]]) {
        // we have to manually reset the offset in p
        p.offset = offset0
        p.buffer(offset0)
      } else {
        // we have to return a HashTrieSet
        val elems1 = p.getAndReset(offset0)
        val bitmap1 = if (elems1.length == elems.length) {
          // we can reuse the original bitmap
          bitmap
        } else {
          // calculate new bitmap by keeping just bits in the kept bitmask
          keepBits(bitmap, kept)
        }
        HashTrieSet(bitmap1, elems1, rs)
      }
    }

    def subsetOf0(that: HashSet[A], level: Int): Boolean = if (that eq this) true
    else that match {
      case that: HashTrieSet[A] if this.size0 <= that.size0 =>
        // create local mutable copies of members
        var abm = this.bitmap
        val a = this.elems
        var ai = 0
        val b = that.elems
        var bbm = that.bitmap
        var bi = 0
        // I tried rewriting this as a tail-recursive function. But the generated jvm bytecode was less than optimal,
        if ((abm & bbm) == abm) {
          // I tried rewriting this using tail recursion, but the generated java byte code was less than optimal
          while(abm!=0) {
            // highest remaining bit in abm
            val alsb = abm ^ (abm & (abm - 1))
            // highest remaining bit in bbm
            val blsb = bbm ^ (bbm & (bbm - 1))
            // if both trees have a bit set at the same position, we need to check the subtrees
            if (alsb == blsb) {
              if (!a(ai).subsetOf0(b(bi), level + 5))
                return false
              // clear lowest remaining one bit in abm and increase the a index
              abm &= ~alsb; ai += 1
            }
            // clear lowermost remaining one bit in bbm and increase the b index
            // we must do this in any case
            bbm &= ~blsb; bi += 1
          }
          true
        } else false
      case _ =>
        // if the other set is a HashTrieSet, but has less elements than this, it can not be a subset
        // if the other set is a HashSet1, we can not be a subset of it because we are a HashTrieSet with at least two children (see assertion)
        // if the other set is a HashSetCollision1, we can not be a subset of it because we are a HashTrieSet with at least two different hash codes
        // if the other set is the empty set, we are not a subset of it because we are not empty
        false
    }

    def foreachLeaf[U](f: (LeafHashSet[A]) => U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreachLeaf(f)
        i += 1
      }
    }
  }

  private def keepBits[A](bitmap: Int, keep: Int): Int = {
    var result = 0
    var current = bitmap
    var kept = keep
    while (kept != 0) {
      // lowest remaining bit in current
      val lsb = current ^ (current & (current - 1))
      if ((kept & 1) != 0) {
        // mark bit in result bitmap
        result |= lsb
      }
      // clear lowest remaining one bit in abm
      current &= ~lsb
      // look at the next kept bit
      kept >>>= 1
    }
    result
  }

  /**
   * Stateful object containing information used by the filter0 method. This also implements A=>Boolean so it can be
   * used as a predicate without creating an additional instance.
   * @param p The predicate for which to filter
   * @param size the maximum size of the result set
   *             This is used to limit the size of the internal buffer
   * @param toss set this to true to negate the predicate
   * @tparam A the element type of the set
   */
  private final class FilterState[A](p: A => Boolean, size:Int, toss:Boolean = false) extends (A => Boolean) {

    /**
     * The buffer used to store nodes when creating new element arrays.
     * Lazily initialized because we might not need it.
     *
     * the maximum size of the buffer is 32 * 7, since we have a maximum of 7 levels with a branching factor
     * of 32 each. But for a small set this might be much too large. For a small set the worst case is all leaves
     * being at the deepest possible level, which gives a required buffer size of n+6 (6 HashTrieSet nodes with one
     * child each).
     */

    //var buffer : Array[HashSet[A]] = null
    val buffer : Array[HashSet[A]] = new Array[HashSet[A]]((size + 6) min (32 * 7))

    /**
     * The current offset into the buffer
     */
    var offset = 0

    /**
     * Add an element to the buffer and increase the offset
     * @param elem
     */
    def add(elem:HashSet[A]) {
      //if(buffer eq null)
      //  buffer = new Array[HashSet[A]]((size + 6) min (32 * 7))
      buffer(offset) = elem
      offset += 1
    }

    /**
     * Get all elements since offset0 and also reset offset to offset0
     * @param offset0 the initial offset
     * @return a copy of elements from offset0 until offset
     */
    def getAndReset(offset0:Int) : Array[HashSet[A]] = {
      val result = new Array[HashSet[A]](offset - offset0)
      System.arraycopy(buffer, offset0, result, 0, result.length)
      offset = offset0
      result
    }

    def apply(value:A) = p(value) ^ toss
  }
}

// helper class for caching buffers for bulk operations
private final class BufferPool[A] {

  private[this] var depth = 0

  private[this] var buffers: Array[Array[HashSet[A]]] = null

  // allocate a 32 element buffer for this level and increase the depth
  def getBuffer(): Array[HashSet[A]] = {
    if (buffers eq null)
      buffers = new Array[Array[HashSet[A]]](7)
    if (buffers(depth) eq null)
      buffers(depth) = new Array[HashSet[A]](32)
    val result = buffers(depth)
    depth += 1
    result
  }

  // decrease the depth
  @inline def freeBuffer() {
    depth -= 1
  }

  @inline def level: Int = depth * 5
}