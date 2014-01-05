/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package immutable4

import scala.annotation.unchecked.{uncheckedVariance => uV}
import generic._
import scala.collection.parallel.immutable.ParHashSet
import scala.annotation.tailrec
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
with immutable.Set[A]
with GenericSetTemplate[A, HashSet]
with SetLike[A, HashSet[A]]
with CustomParallelizable[A, ParHashSet[A]]
with Serializable
{
  import HashSet.{nullToEmpty, LeafHashSet}

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

  override def union(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] =>
      val maxResultSize = this.size + that.size
      val maxBufferSize = (maxResultSize + 6) min (32 * 7)
      val buffer = new Array[HashSet[A]](maxBufferSize)
      nullToEmpty(union0(that, 0, buffer, 0))
    case _ => super.union(that)
  }

  override def intersect(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] =>
      val maxResultSize = this.size min that.size
      val maxBufferSize = (maxResultSize + 6) min (32 * 7)
      val buffer = new Array[HashSet[A]](maxBufferSize)
      nullToEmpty(intersect0(that, 0, buffer, 0))
    case _ => super.intersect(that)
  }

  override def diff(that: GenSet[A]): HashSet[A] = that match {
    case that: HashSet[A] =>
      val maxResultSize = this.size
      val maxBufferSize = (maxResultSize + 6) min (32 * 7)
      val buffer = new Array[HashSet[A]](maxBufferSize)
      nullToEmpty(diff0(that, 0, buffer, 0))
    case _ => super.diff(that)
  }

  /**
   * Union with a leaf HashSet at a given level.
   * @param that a leaf HashSet
   * @param level the depth in the tree. We need this when we have to create a branch node on top of this and that
   * @return The union of this and that at the given level. Unless level is zero, the result is not a self-contained
   *         HashSet but needs to be stored at the correct depth
   */
  private[immutable4] def union0(that: LeafHashSet[A], level: Int): HashSet[A] = that

  /**
   * Union with a HashSet at a given level
   * @param that a HashSet
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The union of this and that at the given level. Unless level is zero, the result is not a self-contained
   *         HashSet but needs to be stored at the correct depth
   */
  private[immutable4] def union0(that: HashSet[A], level:Int, buffer:Array[HashSet[A]], offset0:Int): HashSet[A] = that

  /**
   * Intersection with another hash set at a given level
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The intersection of this and that at the given level. Unless level is zero, the result is not a
   *         self-contained HashSet but needs to be stored at the correct depth
   */
  private[immutable4] def intersect0(that: HashSet[A], level:Int, buffer:Array[HashSet[A]], offset0:Int): HashSet[A] = null

  /**
   * Diff with another hash set at a given level
   * @param level the depth in the tree. We need to keep track of the level to know how deep we are in the tree
   * @param buffer a temporary buffer that is used for temporarily storing elements when creating new branch nodes
   * @param offset0 the first offset into the buffer in which we are allowed to write
   * @return The diff of this and that at the given level. Unless level is zero, the result is not a
   *         self-contained HashSet but needs to be stored at the correct depth
   */
  private[immutable4] def diff0(that: HashSet[A], level:Int, buffer:Array[HashSet[A]], offset0:Int): HashSet[A] = null

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

  sealed abstract class LeafHashSet[A] extends HashSet[A] {
    private[HashSet] def hash:Int
  }

  // TODO: add HashSet2, HashSet3, ...

  class HashSet1[A](private[HashSet] val key: A, private[HashSet] val hash: Int) extends LeafHashSet[A] {
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

    private[immutable4] override def union0(that: LeafHashSet[A], level: Int): HashSet[A] = that match {
      case that if that.hash != this.hash =>
        // different hash code, so there is no need to investigate further.
        // Just create a branch node containing the two.
        makeHashTrieSet(this.hash, this, that.hash, that, level)
      case that: HashSet1[A] =>
        if (this.key == that.key) {
          this
        } else {
          // 32-bit hash collision (rare, but not impossible)
          new HashSetCollision1[A](hash, ListSet.empty + this.key + that.key)
        }
      case that: HashSetCollision1[A] =>
        val ks1 = that.ks + key
        if (ks1.size == that.ks.size) {
          // could this check be made faster by doing an eq check?
          // I am not sure we can rely on ListSet returning itself when the element is already in the set,
          // so it seems unwise to rely on it.
          that
        } else {
          new HashSetCollision1[A](hash, ks1)
        }
    }

    private[immutable4] override def union0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int) = {
      // switch to the Leaf version of union
      // we can exchange the arguments because union is symmetrical
      that.union0(this, level)
    }

    private[immutable4] override def intersect0(that: HashSet[A], level:Int, buffer:Array[HashSet[A]], offset0:Int): HashSet[A] =
      if (that.get0(key, hash, level)) this else null

    private[immutable4] override def diff0(that: HashSet[A], level:Int, buffer:Array[HashSet[A]], offset0:Int): HashSet[A] =
      if (that.get0(key, hash, level)) null else this

    override def removed0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash && key == this.key) HashSet.empty[A] else this

    override def iterator: Iterator[A] = Iterator(key)
    override def foreach[U](f: A => U): Unit = f(key)
  }

  private[immutable4] class HashSetCollision1[A](private[HashSet] val hash: Int, val ks: ListSet[A]) extends LeafHashSet[A] {

    override def size = ks.size

    override def get0(key: A, hash: Int, level: Int): Boolean =
      if (hash == this.hash) ks.contains(key) else false

    override def updated0(key: A, hash: Int, level: Int): HashSet[A] =
      if (hash == this.hash) new HashSetCollision1(hash, ks + key)
      else makeHashTrieSet(this.hash, this, hash, new HashSet1(key, hash), level)

    override def union0(that: LeafHashSet[A], level: Int): HashSet[A] = that match {
      case that if that.hash != this.hash =>
        // different hash code, so there is no need to investigate further.
        // Just create a branch node containing the two.
        makeHashTrieSet(this.hash, this, that.hash, that, level)
      case that: HashSet1[A] =>
        val ks1 = ks + that.key
        if (ks1.size == ks.size) {
          // could this be made faster by doing an eq check?
          // I am not sure we can rely on ListSet returning itself when an element is already in the set,
          // so it seems unwise to rely on it.
          this
        } else {
          // create a new HashSetCollision with the existing hash
          // we don't have to check for size=1 because union is never going to remove elements
          new HashSetCollision1[A](hash, ks1)
        }
      case that: HashSetCollision1[A] =>
        val ks1 = this.ks ++ that.ks
        ks1.size match {
          case size if size == this.ks.size =>
            // could this check be made faster by doing an eq check?
            // I am not sure we can rely on ListSet returning itself when all elements are already in the set,
            // so it seems unwise to rely on it.
            this
          case size if size == that.ks.size =>
            // we have to check this as well, since we don't want to create a new instance if this is a subset of that
            that
          case _ =>
            // create a new HashSetCollision with the existing hash
            // we don't have to check for size=1 because union is never going to remove elements
            new HashSetCollision1[A](hash, ks1)
        }
    }

    override def union0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = that match {
      case that: LeafHashSet[A] =>
        // switch to the simpler Tree/Leaf implementation
        this.union0(that, level)
      case that: HashTrieSet[A] =>
        // switch to the simpler Tree/Leaf implementation
        // we can swap this and that because union is symmetrical
        that.union0(this, level)
      case _ => this
    }

    private[immutable4] override def intersect0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = {
      // filter the keys, taking advantage of the fact that we know their hash code
      val ks1 = ks.filter(that.get0(_, hash, level))
      ks1.size match {
        case 0 =>
          // the empty set
          null
        case size if size == this.size =>
          // unchanged
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          this
        case size if size == that.size =>
          // the other set
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          that
        case 1 =>
          // create a new HashSet1 with the hash we already know
          new HashSet1(ks1.head, hash)
        case _ =>
          // create a new HashSetCollison with the hash we already know and the new keys
          new HashSetCollision1(hash, ks1)
      }
    }

    private[immutable4] override def diff0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = {
      val ks1 = ks.filterNot(that.get0(_, hash, level))
      ks1.size match {
        case 0 =>
          // the empty set
          null
        case size if size == this.size =>
          // unchanged
          // We do this check first since even if the result is of size 1 since
          // it is preferable to return the existing set for better structural sharing
          this
        case 1 =>
          // create a new HashSet1 with the hash we already know
          new HashSet1(ks1.head, hash)
        case _ =>
          // create a new HashSetCollison with the hash we already know and the new keys
          new HashSetCollision1(hash, ks1)
      }
    }

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

    private[immutable4] override def union0(that: LeafHashSet[A], level: Int): HashSet[A] = {
      val index = (that.hash >>> level) & 0x1f
      val mask = (1 << index)
      val offset = Integer.bitCount(bitmap & (mask - 1))
      if ((bitmap & mask) != 0) {
        val sub = elems(offset)
        val sub1 = sub.union0(that, level + 5)
        if (sub eq sub1) this
        else {
          val elems1 = new Array[HashSet[A]](elems.length)
          Array.copy(elems, 0, elems1, 0, elems.length)
          elems1(offset) = sub1
          new HashTrieSet(bitmap, elems1, size + (sub1.size - sub.size))
        }
      } else {
        val elems1 = new Array[HashSet[A]](elems.length + 1)
        Array.copy(elems, 0, elems1, 0, offset)
        elems1(offset) = that
        Array.copy(elems, offset, elems1, offset + 1, elems.length - offset)
        val bitmap1 = bitmap | mask
        new HashTrieSet(bitmap1, elems1, size + that.size)
      }
    }

    private[immutable4] override def union0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes.
        this
      case that: LeafHashSet[A] =>
        // when that is a leaf, we can switch to the simpler Tree/Leaf implementation
        this.union0(that, level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0

        // loop as long as there are bits left in either abm or bbm
        while ((abm | bbm) != 0) {
          // lowest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // lowest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).union0(b(bi), level + 5, buffer, offset)
            rs += sub1.size
            buffer(offset) = sub1
            offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb
            ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb
            bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            val sub1 = a(ai)
            rs += sub1.size
            buffer(offset) = sub1
            offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb
            ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            val sub1 = b(bi)
            rs += sub1.size
            buffer(offset) = sub1
            offset += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb
            bi += 1
          }
        }
        if (rs == this.size) {
          // if the result would be identical to this, we might as well return this
          this
        } else if (rs == that.size) {
          // if the result would be identical to that, we might as well return that
          that
        } else {
          // we don't have to check whether the result is a leaf, since union will only make the set larger
          // and this is not a leaf to begin with.
          val length = offset - offset0
          val elems = new Array[HashSet[A]](length)
          System.arraycopy(buffer, offset0, elems, 0, length)
          new HashTrieSet(this.bitmap | that.bitmap, elems, rs)
        }
      case _ => this
    }

    private[immutable4] override def intersect0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = that match {
      case that if that eq this =>
        // shortcut for when that is this
        // this happens often for nodes deeper in the tree, especially when that and this share a common "heritage"
        // e.g. you have a large set A and do some small operations (adding and removing elements) to it to create B
        // then A and B will have the vast majority of nodes in common, and this eq check will allow not even looking
        // at these nodes!
        this
      case that: LeafHashSet[A] =>
        // when that is a leaf, we can switch to the simpler Tree/Leaf implementation
        // it is OK to swap the arguments because intersect is symmetric
        // (we can't do this in case of diff, which is not symmetric)
        that.intersect0(this, level, buffer, offset0)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // if the bitmasks do not overlap, the result is definitely empty so we can abort here
        if ((abm & bbm) == 0)
          return null

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0
        var rbm = 0

        // loop as long as there are bits left that are set in both abm and bbm
        while ((abm & bbm) != 0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).intersect0(b(bi), level + 5, buffer, offset)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              buffer(offset) = sub1
              offset += 1
            }
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb;
            ai += 1
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb;
            bi += 1
          } else if (unsignedCompare(alsb - 1, blsb - 1)) {
            // alsb is smaller than blsb, or alsb is set and blsb is 0
            // in any case, alsb is guaranteed to be set here!
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb;
            ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb;
            bi += 1
          }
        }

        if (rbm == 0) {
          // if the result bitmap is empty, the result is the empty set
          null
        } else if (rs == size0) {
          // if the result has the same number of elements as this, it must be identical to this,
          // so we might as well return this
          this
        } else if (rs == that.size0) {
          // if the result has the same number of elements as that, it must be identical to that,
          // so we might as well return that
          that
        } else {
          val length = offset - offset0
          if (length == 1 && !buffer(offset0).isInstanceOf[HashTrieSet[A]])
            buffer(offset0)
          else {
            val elems = new Array[HashSet[A]](length)
            System.arraycopy(buffer, offset0, elems, 0, length)
            new HashTrieSet[A](rbm, elems, rs)
          }
        }
      case _ => null
    }

    private[immutable4] override def diff0(that: HashSet[A], level: Int, buffer: Array[HashSet[A]], offset0: Int): HashSet[A] = if (that eq this) null
    else that match {
      case that: HashSet1[A] =>
        removed0(that.key, that.hash, level)
      case that: HashTrieSet[A] =>
        val a = this.elems
        var abm = this.bitmap
        var ai = 0

        val b = that.elems
        var bbm = that.bitmap
        var bi = 0

        // fetch a new temporary array that is guaranteed to be big enough (32 elements)
        var offset = offset0
        var rs = 0
        var rbm = 0

        // loop until there are no more bits in abm
        while(abm!=0) {
          // highest remaining bit in abm
          val alsb = abm ^ (abm & (abm - 1))
          // highest remaining bit in bbm
          val blsb = bbm ^ (bbm & (bbm - 1))
          if (alsb == blsb) {
            val sub1 = a(ai).diff0(b(bi), level + 5, buffer, offset)
            if (sub1 ne null) {
              rs += sub1.size
              rbm |= alsb
              buffer(offset) = sub1; offset += 1
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
            buffer(offset) = sub1; offset += 1
            // clear lowest remaining one bit in abm and increase the a index
            abm &= ~alsb; ai += 1
          } else {
            // blsb is smaller than alsb, or blsb is set and alsb is 0
            // in any case, blsb is guaranteed to be set here!
            // clear lowest remaining one bit in bbm and increase the b index
            bbm &= ~blsb; bi += 1
          }
        }
        if (rbm == 0) {
          null
        } else if(rs == this.size0)
        // if the result has the same number of elements as this, it must be identical to this,
        // so we might as well return this
          this
        else {
          val length = offset - offset0
          if (length == 1 && !buffer(offset0).isInstanceOf[HashTrieSet[A]])
            buffer(offset0)
          else {
            val elems = new Array[HashSet[A]](length)
            System.arraycopy(buffer, offset0, elems, 0, length)
            new HashTrieSet[A](rbm, elems, rs)
          }
        }
      case that: HashSetCollision1[A] =>
        // we remove the elements using removed0 so we can use the fact that we know the hash of all elements
        // to be removed
        @tailrec def removeAll(s:HashSet[A], r:ListSet[A]) : HashSet[A] =
          if(r.isEmpty || (s eq null)) s
          else removeAll(s.removed0(r.head, that.hash, level), r.tail)
        val result = removeAll(this, that.ks)
        // workaround since remove0 currently does not do proper structural sharing with colliding keys
        if(result.size == size)
          this
        else
          result
      case _ => this
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

    override def iterator = ???

    override def foreach[U](f: A =>  U): Unit = {
      var i = 0
      while (i < elems.length) {
        elems(i).foreach(f)
        i += 1
      }
    }
  }

  @inline private def nullToEmpty[A](s: HashSet[A]): HashSet[A] = if (s eq null) empty[A] else s

  // unsigned comparison
  @inline private[this] def unsignedCompare(i: Int, j: Int) =
    (i < j) ^ (i < 0) ^ (j < 0)

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

