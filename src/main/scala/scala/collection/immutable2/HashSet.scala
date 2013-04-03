package scala.collection.immutable2

import collection.immutable.ListSet
import collection.{GenTraversableOnce, GenSet, SetLike}
import collection.generic.{CanBuildFrom, ImmutableSetFactory}
import annotation.tailrec

/**
 * An efficient immutable set
 * @tparam A the element type. invariant, unfortunately
 */
sealed abstract class HashSet[A] extends Set[A] with SetLike[A, HashSet[A]] {

  import HashSet.{nullToEmpty, BufferPool, LeafHashSet, HashSet1}

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

  override def filter(p: (A) => Boolean): HashSet[A] = nullToEmpty(filter0(p, new BufferPool[A]()))

  override def iterator: Iterator[A]

  override def size: Int

  override def foreach[U](f: A => U): Unit

  def contains0(key: A, hash: Int, level: Int): Boolean

  protected def subsetOf0(that: HashSet[A], level: Int): Boolean

  protected def union0(that: LeafHashSet[A], level: Int): HashSet[A]

  protected def removed0(key: A, hash: Int, level: Int): HashSet[A]

  protected def filter0(p: A => Boolean, pool: BufferPool[A]): HashSet[A]

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

  // helper class for caching buffers for bulk operations
  private final class BufferPool[A] {

    private[this] var depth = 0

    private[this] var buffers: Array[Array[HashSet[A]]] = null

    // allocate a 32 element buffer for this level and increase the depth
    def getBuffer(): Array[HashSet[A]] = {
      if (buffers eq null)
        buffers = (new Array[Array[HashSet[A]]](7))
      if (buffers(depth) eq null)
        buffers(depth) = (new Array[HashSet[A]](32))
      val result = buffers(depth)
      depth += 1
      result
    }

    // decrease the depth
    def freeBuffer() {
      depth -= 1
    }

    def level: Int = depth * 5
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

  private def nullToEmpty[A](s: HashSet[A]): HashSet[A] = if (s eq null) empty[A] else s

  private final class EmptySet[A] extends HashSet[A] {

    override def size = 0

    override def iterator: Iterator[A] = Iterator.empty

    override def foreach[U](f: A => U) {}

    def contains0(key: A, hash: Int, level: Int): Boolean = false

    def subsetOf0(that: HashSet[A], level: Int): Boolean = true

    def union0(that: LeafHashSet[A], level: Int): HashSet[A] = that

    def removed0(key: A, hash: Int, level: Int): HashSet[A] = null

    def filter0(p: A => Boolean, pool: BufferPool[A]): HashSet[A] = null

    def union0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = that

    def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = null

    def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = null
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

    def filter0(p: A => Boolean, pool: BufferPool[A]): HashSet[A] =
      if (p(key)) this else null

    def union0(that: HashSet[A], pool: BufferPool[A]) =
      that.union0(this, pool.level)
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

    def filter0(p: A => Boolean, pool: BufferPool[A]): HashSet[A] = {
      val ks1 = ks.filter(p)
      if (ks1.isEmpty)
        null
      else if (ks1.tail.isEmpty)
        new HashSet1(ks1.head, hash)
      else if (ks1.size == this.size)
        this
      else
        new HashSetCollision1(hash, ks1)
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
  }

  final case class HashTrieSet[A](bitmap: Int, elems: Array[HashSet[A]], size0: Int) extends HashSet[A] {

    // assert(size0 == elems.map(_.size).sum)
    assert(Integer.bitCount(bitmap) == elems.length)
    assert(elems.length > 1 || (elems.length == 1 && elems(0).isInstanceOf[HashTrieSet[_]]))
    assert(size0 > 1)

    private[this] def truncate[A](buffer: Array[HashSet[A]], length: Int) = {
      val elems = new Array[HashSet[A]](length)
      var i = 0
      while (i < elems.length) {
        elems(i) = buffer(i)
        i += 1
      }
      elems
    }

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
        null
      else if (length == 1 && !buffer(0).isInstanceOf[HashSet.HashTrieSet[_]])
        buffer(0)
      else
        new HashTrieSet[A](bitmap, truncate(buffer, length), size)
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

    /*
    def filter0(p: A => Boolean, pool: BufferPool[A]): HashSet[A] = {
      var i = 0
      var ir = 0
      var rbm = 0
      var rs = 0
      val r = pool.getBuffer()
      // iterate over all 32 masks even though just a few of them might be occupied.
      // this iteration is much cheaper than calling Integer.bitCount
      var mask = 1
      while (mask != 0) {
        if ((bitmap & mask) != 0) {
          val sub = elems(i)
          val subNew = sub.filter0(p, pool)
          if (subNew ne null) {
            r(ir) = subNew
            rs += subNew.size
            rbm |= mask
            ir += 1
          }
          // increase the offset for each occupied slot. That way we don't have to invoke Integer.bitCount
          i += 1
        }
        mask <<= 1
      }

      pool.freeBuffer()
      if (size0 == rs)
        this
      else
        newInstance(rbm, r, ir, rs)
    } */

    def union0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = if (that eq this) this
    else that match {
      case that: HashSet1[A] =>
        this.union0(that, pool.level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        val b = that.elems
        val abm = this.bitmap
        val bbm = that.bitmap

        // construct a new array of appropriate size
        val r = pool.getBuffer()

        // run through both bitmaps and add elements to it
        var ir = 0
        var ia = 0
        var ib = 0
        var rs = 0
        var mask = 1
        while (mask != 0) {
          val aset = (abm & mask) == mask
          val bset = (bbm & mask) == mask
          if (aset && bset) {
            val subNew = a(ia).union0(b(ib), pool)
            r(ir) = subNew
            rs += subNew.size
            ir += 1
            ia += 1
            ib += 1
          } else if (aset) {
            val subNew = a(ia)
            r(ir) = subNew
            rs += subNew.size
            ir += 1
            ia += 1
          } else if (bset) {
            val subNew = b(ib)
            r(ir) = subNew
            rs += subNew.size
            ir += 1
            ib += 1
          }
          mask <<= 1
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
          newInstance(this.bitmap | that.bitmap, r, ir, rs)
      case hs: HashSetCollision1[A] => that.union0(this, pool)
      case _ => this
    }

    def intersect0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = if (that eq this) this
    else that match {
      case that: HashSet1[A] =>
        that.intersect0(this, pool)
      case that: HashTrieSet[A] =>
        val a = this.elems
        val b = that.elems
        val abm = this.bitmap
        val bbm = that.bitmap
        var rbm = 0
        var ia = 0
        var ib = 0
        var ir = 0
        var rs = 0

        val r = pool.getBuffer()
        var mask = 1
        while (mask != 0) {
          val aset = (abm & mask) == mask
          val bset = (bbm & mask) == mask
          if (aset && bset) {
            val subNew = a(ia).intersect0(b(ib), pool)
            if (subNew ne null) {
              r(ir) = subNew
              rbm |= mask
              rs += subNew.size
              ir += 1
            }
            ia += 1
            ib += 1
          } else if (aset) {
            ia += 1
          } else if (bset) {
            ib += 1
          }
          mask <<= 1
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
          newInstance(rbm, r, ir, rs)
      case hs: HashSetCollision1[_] => that.intersect0(this, pool)
      case _ => null
    }

    def diff0(that: HashSet[A], pool: BufferPool[A]): HashSet[A] = if (that eq this) null
    else that match {
      case hs: HashSet1[A] =>
        removed0(hs.key, hs.hash, pool.level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        val b = that.elems
        val abm = this.bitmap
        val bbm = that.bitmap
        var rbm = 0
        var ia = 0
        var ib = 0
        var ir = 0
        var rs = 0

        // construct a new array of maximum possible size
        val r = pool.getBuffer()
        var mask = 1
        while (mask != 0) {
          val aset = (abm & mask) == mask
          val bset = (bbm & mask) == mask
          if (aset && bset) {
            val subNew = a(ia).diff0(b(ib), pool)
            if (subNew ne null) {
              r(ir) = subNew
              rbm |= mask
              rs += subNew.size
              ir += 1
            }
            ia += 1
            ib += 1
          } else if (aset) {
            val subNew = a(ia)
            r(ir) = subNew
            rs += subNew.size
            rbm |= mask
            ir += 1
            ia += 1
          }
          else if (bset) {
            ib += 1
          }
          mask <<= 1
        }
        pool.freeBuffer()
        if(rs == this.size0)
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        else
          newInstance(rbm, r, ir, rs)
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

    def filter0(p: A => Boolean, pool: BufferPool[A]): HashSet[A] = {
      val a = this.elems
      // state for iteration over a
      var abm = this.bitmap
      var ai = 0

      // state for result buffer
      val r = pool.getBuffer()
      var rbm = 0
      var ri = 0
      var rs = 0
      // iterate over all one bits in abm
      while (abm!=0) {
        // highest remaining bit in abm
        val alsb = abm ^ (abm & (abm - 1))
        // filter the subnode (will return null as the empty node)
        val sub1 = a(ai).filter0(p, pool)
        // if we have a result (might be the original node, we don't care)
        if(sub1 ne null) {
          // store node in result buffer
          r(ri) = sub1
          // mark bit in result bitmap
          rbm |= alsb
          // increase result buffer index
          ri += 1
          // add result size
          rs += sub1.size
        }
        // clear lowest remaining one bit in abm and increase the a index
        abm &= ~alsb
        ai += 1
      }

      pool.freeBuffer()
      // if the size is the same, it must be the same node, so ignore what is in r
      if (size0 == rs)
        this
      else
        newInstance(rbm, r, ri, rs)
    }

    def subsetOf0(that: HashSet[A], level: Int): Boolean = if (that eq this) true
    else that match {
      case that: HashTrieSet[A] =>
        // create local copies of bitmap members
        var abm = this.bitmap
        var ia = 0
        val a = this.elems
        var bbm = that.bitmap
        var ib = 0
        val b = that.elems
        if ((abm & bbm) == abm) {
          // I tried rewriting this using tail recursion, but the generated java byte code was less than optimal
          while(abm!=0) {
            // highest remaining bit in abm
            val alsb = abm ^ (abm & (abm - 1))
            // highest remaining bit in bbm
            val blsb = bbm ^ (bbm & (bbm - 1))
            // if both trees have a bit set at the same position, we need to check the subtrees
            if (alsb == blsb) {
              if (!a(ia).subsetOf0(b(ib), level + 5))
                return false
              // clear lowest remaining one bit in abm and increase the a index
              abm &= ~alsb; ia += 1
              // clear lowest remaining one bit in bbm and increase the b index
              bbm &= ~blsb; ib += 1
            } else {
              // b must always have more bits set than a (we have made sure that abm is a subset of bbm above)
              assert(unsignedCompare(blsb, alsb))
              // clear lowermost remaining one bit in bbm and increase the b index
              bbm &= ~blsb; ib += 1
            }
          }
          true
        } else false
      case _ =>
        // if the other set is a HashSet1, we can not be a subset of it because we are a HashTrieSet with at least two children (see assertion)
        // if the other set is a HashSetCollision1, we can not be a subset of it because we are a HashTrieSet with at least two different hash codes
        // if the other set is the empty set, we are not a subset of it because we are not empty
        false
    }
  }

}
