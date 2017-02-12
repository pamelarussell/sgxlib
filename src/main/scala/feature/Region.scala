package feature

import scala.collection
import scala.collection.mutable.ListBuffer

/** A genomic region.
  *
  * Can include zero, one, or multiple blocks on a chromosome. Includes an orientation.
  * Does not include any further information.
  *
  */
sealed abstract class Region extends Ordered[Region] {

  /** Returns a boolean value representing whether this [[Region]] overlaps another [[Region]].
    *
    * In order to overlap, the two [[Region]]s must be on the same chromosome, have compatible
    * [[Orientation]]s, and have some pair of respective [[Block]]s with overlapping spans.
    *
    * @param o Other [[Region]]
    * @return True if the [[Region]]s overlap, false otherwise
    */
  def overlaps(o: Region): Boolean

  /** Returns a boolean value representing whether this [[Region]] overlaps another [[Region]]
    * and their introns are compatible.
    *
    * That is, no INTERNAL block boundary for one of the [[Region]]s can fall strictly within a
    * [[Block]] of the other [[Region]].
    *
    * @param o Other [[Region]]
    * @return True if the [[Region]]s overlap and their introns are compatible,
    *         false otherwise
    */
  def overlapsCompatibleIntrons(o: Region): Boolean

  /** Returns a boolean value representing whether this [[Region]] contains another [[Region]]
    * and their introns are compatible.
    *
    * That is, no INTERNAL block boundary for one of the [[Region]]s can fall strictly within a
    * [[Block]] of the other [[Region]].
    *
    * @param o Other [[Region]]
    * @return True if this [[Region]] contains the other and their introns are compatible,
    *         false otherwise
    */
  def containsCompatibleIntrons(o: Region): Boolean =
    overlapsCompatibleIntrons(o) && o.start >= start && o.end <= end

  /** Returns a boolean value representing whether this [[Region]] contains another [[Region]].
    *
    * In order to contain the other, the two [[Region]]s must be on the same chromosome, have
    * compatible [[Orientation]]s, and every block of the other [[Region]] must be fully contained in
    * the span of a block of this [[Region]].
    *
    * @param o Other [[Region]]
    * @return True if this [[Region]] contains the other, false otherwise
    */
  def contains(o: Region): Boolean

  /** Returns a [[Region]] representing the union of this with another [[Region]].
    *
    * If the two [[Region]]s are on the same chromosome and have compatible [[Orientation]]s,
    * the blocks will be merged and assigned the consensus [[Orientation]] ([[Orientation.consensus]])
    * for the final returned [[Region]].
    *
    * If this or the other [[Region]] is [[Empty]], the non-empty [[Region]] will be
    * returned, or if both are [[Empty]], [[Empty]] will be returned.
    *
    * Otherwise, an exception is thrown.
    *
    * @param o Other [[Region]]
    * @return [[Region]] representing the union of this with the other [[Region]]
    */
  def union(o: Region): Region

  /** Returns a [[Region]] representing the intersection of this with another [[Region]].
    *
    * If the two [[Region]]s overlap ([[Region.overlaps]]), a [[Region]] is returned whose
    * blocks are the disjoint sections of overlap between the blocks of the two original [[Region]]s,
    * and whose [[Orientation]] is the consensus ([[Orientation.consensus]]) of those of the two
    * original [[Region]]s. If the two [[Region]]s do not overlap, [[Empty]] is returned.
    *
    * @param o Other [[Region]]
    * @return [[Region]] representing the intersection of this with the other [[Region]]
    */
  def intersection(o: Region): Region

  /** Returns a [[Region]] representing this minus the overlap with another [[Region]].
    *
    * If the two [[Region]]s overlap ([[Region.overlaps]]), a [[Region]] is returned whose
    * blocks are the sections of this [[Region]]'s blocks that are not part of the overlap.
    * Otherwise, this is returned. In either case, the returned [[Region]] has the same
    * [[Orientation]] as this [[Region]].
    *
    * @param o Other [[Region]]
    * @return [[Region]] representing this minus the other [[Region]]
    */
  def minus(o: Region): Region

  /** Returns a boolean value representing whether this [[Region]] is empty.
    *
    * The only possible way for a region to be empty (have no blocks) is for it
    * to be equal to [[Empty]].
    */
  def isEmpty: Boolean

  /** Returns the name of the chromosome on which this [[Region]] is located. */
  def chr: String

  /** Returns the zero-based start position of this region.
    *
    * Regardless of [[Orientation]], this is the smallest (leftmost) position
    * of the region in chromosome coordinates. The start position is inclusive.
    * (All coordinate intervals are half open. All genomic positions are zero-based.)
    */
  def start: Int

  /** Returns the zero-based end position of this region.
    *
    * Regardless of [[Orientation]], this one plus the largest (rightmost)
    * position included in the region span in chromosome coordinates.
    * That is to say, the end position is exclusive.
    * (All coordinate intervals are half-open. All genomic positions are zero-based.)
    *
    * @return
    */
  def end: Int

  /** Returns the [[Orientation]] of this region. */
  def orientation: Orientation

  /** Returns a list of the [[Block]]s of this [[Region]].
    *
    * Regardless of [[Orientation]], the returned list is in sorted order by chromosome coordinates.
    */
  def blocks: List[Block]

  /** Returns the number of [[Block]]s in this [[Region]].
    *
    * [[Empty]] has zero [[Block]]s.
    */
  def numBlocks: Int

  /** Returns a merged [[Region]] representing this plus an additional [[Block]].
    *
    * The new [[Block]] is merged into this [[Region]] as in the [[Region.union]] method.
    *
    * If this and the new [[Block]] have incompatible [[Orientation]]s or are on different
    * chromosomes, an exception is thrown.
    *
    * @param block New [[Block]] to add
    * @return [[Region]] representing this with the additional [[Block]] merged in
    */
  def addBlock(block: Block): Region

  /** Returns a list of [[Block]]s representing the introns of this [[Region]].
    *
    * The returned list is in order from left to right. Each [[Block]] in the returned list is the span of a gap
    * between two [[Block]]s of this [[Region]]. The returned [[Block]]s have the same [[Orientation]]
    * as this [[Region]]. The start position of each returned [[Block]] is equal to the end position (exclusive)
    * of the previous adjacent [[Block]] in this [[Region]]. The end position of each returned [[Block]] (exclusive)
    * is equal to the start position of the following adjacent [[Block]] in this [[Region]].
    *
    * If this [[Region]] is empty or has only one [[Block]], Nil is returned.
    *
    * @return A list of [[Block]]s representing the gaps between the [[Block]]s of this [[Region]], or Nil if this
    *         [[Region]] has less than two [[Block]]s.
    */
  def getIntrons: List[Block] = {
    if(numBlocks < 2) Nil
    else {
      val blockStarts: List[Int] = blocks.map(b => b.start)
      val blockEnds: List[Int] = blocks.map(b => b.end)
      blockEnds.zip(blockStarts.tail).map(p => Block(chr, p._1, p._2, orientation))
    }
  }


  /** Returns a [[Region]] resulting from trimming back the start and end positions of this [[Region]].
    *
    * This [[Region]] is truncated to the new start and end positions in chromosome coordinates.
    * This may mean some [[Block]]s are removed altogether, some are shortened, and some
    * are left intact. If the new start or end position is outside the span of this [[Region]],
    * that parameter will not affect the returned [[Region]].
    *
    * As always, start positions are inclusive and end positions are exclusive.
    *
    * @param newStart New zero-based start position in chromosome coordinates
    * @param newEnd New zero-based end position in chromosome coordinates
    * @return [[Region]] representing this with truncated start and end positions
    */
  def trim(newStart: Int, newEnd: Int): Region = {
    if(newStart > newEnd) throw new IllegalArgumentException("New start must be <= new end")
    if(newStart < 0) throw new IllegalArgumentException("New start must be >= 0")
    intersection(Block(chr, newStart, newEnd, orientation))
  }

  /** Returns the sum of the lengths of the [[Block]]s of this [[Region]].
    *
    * [[Block]] length is the end position minus the start position, where, as always,
    * start position is inclusive and end position is exclusive. Therefore, size is
    * the number of individual genomic positions included in the [[Region]].
    *
    * The size of [[Empty]] is zero.
    *
    * @return The sum of the lengths of the [[Block]]s of this [[Region]]
    */
  def size: Int = blocks.map(b => b.end - b.start).sum

  /** Returns the position relative to this [[Region]] accounting for splicing and [[Orientation]].
    *
    * The returned position counts with respect to the "beginning" or "5-prime end" of this
    * [[Region]]. It counts along the [[Block]]s only, ignoring intervening areas ("introns").
    *
    * For example, if this [[Region]] has [[Orientation]] [[Plus]], the start position
    * would map to position zero by this method, and the last position (the end position minus one)
    * would map to position [[Region.size]] minus one. If this [[Region]] has [[Orientation]] [[Minus]],
    * the last position of the [[Region]] (the end position minus one) would map to position zero
    * by this method, and the start position would map to position [[Region.size]] minus one.
    *
    * This [[Region]] must have [[Orientation]] [[Plus]] or [[Minus]]. Otherwise an exception is thrown.
    *
    * If the requested chromosome position does not lie within one of the [[Block]]s of this [[Region]],
    * [[None]] is returned.
    *
    * @param chrPos Zero-based position in chromosome coordinates
    * @return The zero-based converted position with respect to this [[Region]], or [[None]] if the requested
    *         position does not overlap a [[Block]] of this [[Region]]
    */
  def relativePos(chrPos: Int): Option[Int]

  /** Returns the position in chromosome coordinates corresponding to a relative position along this [[Region]].
    *
    * The requested relative position counts with respect to the "beginning" or "5-prime end" of this
    * [[Region]]. It counts along the [[Block]]s only, ignoring intervening areas ("introns").
    *
    * For example, if this [[Region]] has [[Orientation]] [[Plus]], the relative position zero
    * would map to the start position in chromosome coordinates by this method, and the relative
    * position [[Region.size]] minus one would map to the last position (the end position minus one)
    * in chromosome coordinates. If this [[Region]] has [[Orientation]] [[Minus]],
    * position zero would map to the last position of the [[Region]] (the end position minus one)
    * by this method, and [[Region.size]] minus one would map to the start position.
    *
    * This [[Region]] must have [[Orientation]] [[Plus]] or [[Minus]]. The requested relative position
    * must be between zero (inclusive) and [[Region.size]] (exclusive). Otherwise, an exception is thrown.
    *
    * @param relativePos Zero-based relative position with respect to this [[Region]]
    * @return The zero-based position in chromosome coordinates
    */
  def chrPos(relativePos: Int): Int

}

private object Region {

  // Static function to get union between two blocks
  def union(b1: Block, b2: Block): Region = {
    if(!Orientation.isCompatible(b1, b2)) throw new IllegalArgumentException("Orientations must be compatible")
    if(b1.chr != b2.chr) throw new IllegalArgumentException("Blocks must have same chromosome")
    val or = Orientation.consensus(b1.orientation, b2.orientation)
    if(b1.overlaps(b2) || adjacent(b1, b2)) {
      Block(b1.chr, scala.math.min(b1.start, b2.start), scala.math.max(b1.end, b2.end), or)
    }
    else {
      if(b1.start < b2.start) {
        BlockSet(List(
          Block(b1.chr, b1.start, b1.end, or),
          Block(b2.chr, b2.start, b2.end, or)
        ))
      } else {
        BlockSet(List(
          Block(b2.chr, b2.start, b2.end, or),
          Block(b1.chr, b1.start, b1.end, or)
        ))
      }
    }
  }

  // Static function to get intersection between two blocks
  def intersection(b1: Block, b2: Block): Region = {
    if(b1.overlaps(b2)) {
      Block(b1.chr, scala.math.max(b1.start, b2.start), scala.math.min(b1.end, b2.end),
        Orientation.consensus(b1.orientation, b2.orientation))
    } else Empty
  }

  // Static function to get union between a block and block set
  def union(b: Block, bs: BlockSet): Region = {

    // Check for compatible orientations and chromosomes, then save consensus
    if(!Orientation.isCompatible(b, bs)) throw new IllegalArgumentException("Incompatible orientations")
    if(b.chr != bs.chr) throw new IllegalArgumentException("Different chromosomes")
    val strand = Orientation.consensus(b.orientation, bs.orientation)
    val chr: String = b.chr

    val it: Iterator[Block] = bs.blocks.iterator
    val rtrnBlks: collection.mutable.MutableList[Block] = collection.mutable.MutableList.empty // Blocks of the returned region
    var added, overlap: Boolean = false
    var overlapStart, overlapEnd: Int = -1
    while(it.hasNext) {
      val blk = it.next()
      if(overlap) { // Already in an overlap
        if(b.overlaps(blk) || b.end == blk.start) {
          overlapEnd = math.max(overlapEnd, blk.end)
        } else {
          rtrnBlks += Block(chr, overlapStart, overlapEnd, strand)
          rtrnBlks += Block(chr, blk.start, blk.end, strand)
          overlap = false
          added = true
        }
      } else { // Not already  in an overlap
        if(b.end < blk.start) { // b is entirely before blk
          if(!added) rtrnBlks += Block(chr, b.start, b.end, strand)
          rtrnBlks += Block(chr, blk.start, blk.end, strand)
          added = true
        } else if(b.start > blk.end) { // b is entirely after blk
          rtrnBlks += Block(chr, blk.start, blk.end, strand)
        } else { // New overlap
          overlap = true
          overlapStart = math.min(b.start, blk.start)
          overlapEnd = math.max(b.end, blk.end)
        }
      }
    }
    if(overlap) {
      rtrnBlks += Block(chr, overlapStart, overlapEnd, strand)
      overlap = false
      added = true
    }
    if(!added) rtrnBlks += Block(chr, b.start, b.end, strand)
    if(rtrnBlks.length == 1) rtrnBlks.head
    else BlockSet(rtrnBlks.toList)
  }

  // Static function to get intersection between a block and block set
  def intersection(b: Block, bs: BlockSet): Region = {
    val it: Iterator[Block] = bs.blocks.iterator
    val rtrnBlks: collection.mutable.MutableList[Block] = collection.mutable.MutableList.empty // Blocks of the returned region
    while(it.hasNext) {
      val blk = it.next()
      val is = b.intersection(blk)
      is match {
        case Empty => Unit
        case isBlk : Block => rtrnBlks += Block(isBlk.chr, isBlk.start, isBlk.end, isBlk.orientation)
        case _ => throw new IllegalArgumentException("Not supported")
      }
    }
    if(rtrnBlks.isEmpty) Empty
    else if(rtrnBlks.size == 1) rtrnBlks.head
    else BlockSet(rtrnBlks.toList)
  }

  // Static function to subtract a block from another
  def minus(b1: Block, b2: Block): Region = {
    if(!Orientation.isCompatible(b1, b2)) b1
    else if(!b1.overlaps(b2)) b1
    else if(b2.contains(b1)) Empty
    else if(b1.contains(b2)) { // b1 contains b2
      if(b1.start == b2.start) { // b1 contains b2 and they have equal start positions
        // b1 contains b2, equal start positions, different end positions
        Block(b1.chr, b2.end, b1.end, b1.orientation)
      } else { // b1 contains b2 and they have different start positions
        if(b1.end == b2.end) { // b1 contains b2, different start positions, same end position
          Block(b1.chr, b1.start, b2.start, b1.orientation)
        } else { // b1 contains b2, different start and end positions
          BlockSet(List(Block(b1.chr, b1.start, b2.start, b1.orientation),
            Block(b1.chr, b2.end, b1.end, b1.orientation)))
        }
      }
    } else { // b1 and b2 overlap, neither is contained in the other
      if(b1.start < b2.start) Block(b1.chr, b1.start, b2.start, b1.orientation)
      else Block(b1.chr, b2.end, b1.end, b1.orientation)
    }
  }

  // Static function to determine whether two blocks are adjacent but not overlapping
  def adjacent(b1: Block, b2: Block): Boolean = b1.chr == b2.chr && (b1.start == b2.end || b1.end == b2.start)

  // Static block compare function
  def compare(b1: Block, b2: Block): Int = {
    // First compare chromosomes
    val cc = scala.math.Ordering.String.compare(b1.chr, b2.chr)
    if(cc != 0) cc
    else {
      // Next compare start positions
      val sc = b1.start - b2.start
      if(sc != 0) sc
      else {
        // Next compare end positions
        val ec = b1.end - b2.end
        // Finally compare orientations
        if(ec != 0) ec
        else Orientation.ArbitraryOrdering.compare(b1.orientation, b2.orientation)
      }
    }
  }

  // Static compare function for block and block set
  def compare(b: Block, bs: BlockSet): Int = {
    // First compare span
    val cb = compare(b, Block(bs.chr, bs.start, bs.end, bs.orientation))
    if(cb != 0) cb
    else -1 // If same span, block is less than block set
  }

  // Static compare function for block set and block
  def compare(bs: BlockSet, b: Block): Int = -1 * compare(b, bs)

  // Static block set compare function
  def compare(bs1: BlockSet, bs2: BlockSet): Int = {
    // First compare on span only
    val cb: Int = compare(Block(bs1.chr, bs1.start, bs1.end, bs1.orientation),
      Block(bs2.chr, bs2.start, bs2.end, bs2.orientation))
    if(cb != 0) cb
    else { // Block sets have same span and orientation
    // Compare number of blocks
    val cn: Int = bs1.numBlocks - bs2.numBlocks
      if(cn != 0) cn
      else { // Same number of blocks
      // Return comparison of first pair of different blocks
      val db: List[(Block, Block)] = (bs1.blocks zip bs2.blocks).dropWhile(bp => compare(bp._1, bp._2) == 0)
        if(db.isEmpty) 0
        else compare(db.head._1, db.head._2)
      }
    }
  }

}

/** An empty region. [[Empty]] has no chromosome, [[Block]]s, or [[Orientation]]. */
case object Empty extends Region {

  /** Returns false. */
  override def overlaps(feat: Region): Boolean = false

  /** Returns false. */
  override def overlapsCompatibleIntrons(o: Region): Boolean = false

  /** Returns the other [[Region]]. */
  override def union(feat: Region): Region = feat

  /** Returns [[Empty]]. */
  override def intersection(feat: Region): Region = Empty

  /** Returns [[Empty]]. */
  override def minus(feat: Region): Region = Empty

  /** Returns true. */
  override def isEmpty: Boolean = true

  /** Throws an IllegalStateException. */
  override def chr: String = throw new IllegalStateException("Empty region")

  /** Throws an IllegalStateException. */
  override def start: Int = throw new IllegalStateException("Empty region")

  /** Throws an IllegalStateException. */
  override def end: Int = throw new IllegalStateException("Empty region")

  /** Throws an IllegalStateException. */
  override def orientation: Orientation = throw new IllegalStateException("Empty region")

  /** Returns Nil. */
  override def blocks: List[Block] = Nil

  /** Returns the [[Block]]. */
  override def addBlock(block: Block): Region = block

  /** Returns false. */
  override def contains(feat: Region): Boolean = false

  /** Returns zero. */
  override val numBlocks: Int = 0

  /** Returns [[Empty]]. */
  override def trim(newStart: Int, newEnd: Int): Region = Empty

  /** Returns "Empty". */
  override def toString: String = "Empty"

  /** Returns zero if other [[Region]] is [[Empty]]; a positive integer otherwise. */
  override def compare(that: Region): Int = that match {
    case Empty => 0
    case _ => 1
  }

  /** Returns None. */
  override def relativePos(chrPos: Int): Option[Int] = None

  /** Throws an IllegalStateException. */
  override def chrPos(relativePos: Int): Int = throw new IllegalStateException("Empty region")

}

/** A single contiguous block on a chromosome with an [[Orientation]].
  *
  * @param chr Chromosome name. Cannot start with "chr" (otherwise an [[IllegalArgumentException]] is thrown).
  *            All code in this library automatically strips "chr" from feature chromosome names before
  *            instantiating [[Block]]s. For example, [[format.GTF22Record]] strips "chr" from GTF2.2 lines
  *            before creating [[Feature]]s. Client code that implements new ways to create features will need
  *            to do this as well.
  * @param start Zero-based start position (inclusive)
  * @param end Zero-based end position (exclusive)
  * @param orientation Block orientation
  */
final case class Block(chr: String, start: Int, end: Int, orientation: Orientation) extends Region {

  if(start < 0) throw new IllegalArgumentException("Start must be nonnegative")
  if(end <= start) throw new IllegalArgumentException(s"End ($end) must be greater than start ($start). Otherwise use empty region.")
  if(chr.isEmpty) throw new IllegalArgumentException("Chromosome name must not be empty")
  if(chr.startsWith("chr")) throw new IllegalArgumentException("Chromosome name must not start with \"chr\"")

  override def overlaps(feat: Region): Boolean = feat match {
    case Empty => false
    case Block(c, s, e, o) =>
      if(chr != c) false
      else if(!Orientation.isCompatible(orientation, o)) false
      else (start < e && e <= end) || (s < end && end <= e)
    case bs: BlockSet => bs.overlaps(this)
  }

  override def overlapsCompatibleIntrons(o: Region): Boolean = o match {
      case Empty => false
      case b: Block => overlaps(b)
      case bs: BlockSet => bs.overlapsCompatibleIntrons(this)
    }

  override def union(feat: Region): Region = feat match {
    case Empty => this
    case b: Block => Region.union(this, b)
    case bs: BlockSet => Region.union(this, bs)
  }

  override def intersection(feat: Region): Region = feat match {
    case Empty => Empty
    case b: Block => Region.intersection(this, b)
    case bs: BlockSet => Region.intersection(this, bs)
  }

  override def minus(feat: Region): Region = {
    if(feat.isEmpty) this
    else if(!Orientation.isCompatible(orientation, feat.orientation)) this
    else feat match {
      case Empty => this
      case b: Block => Region.minus(this, b)
      case BlockSet(bs) => bs.foldLeft[Region](this)((f, b) => f.minus(b))
    }
  }

  override def isEmpty: Boolean = false

  override def blocks: List[Block] = List(this)

  override def addBlock(block: Block): Region = Region.union(this, block)

  override def contains(feat: Region): Boolean = feat match {
    case Empty => false
    case f: Region => Orientation.isCompatible(this, f) &&
      chr == f.chr &&
      f.start >= start &&
      f.end <= end
  }

  override def numBlocks: Int = 1

  /** Returns a string representation of this [[Block]]. */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    sb.append(chr)
    sb.append(':')
    sb.append(start)
    sb.append('-')
    sb.append(end)
    sb.append(':')
    sb.append(orientation.toString)
    sb.append(']')
    sb.toString()
  }

  /** Returns the result of comparing this with another [[Region]].
    *
    * If other [[Region]] is [[Empty]], returns a negative integer.
    *
    * If other [[Region]] is a [[Block]], first compare chromosome, then start position, then end position, then [[Orientation]]
    * according to [[Orientation.ArbitraryOrdering]]. Return the first non-zero comparison. If all are equal, return zero.
    *
    * If other [[Region]] is a [[BlockSet]], first compare spans ignoring introns, as if both were [[Block]]s. If they have the same
    * span, return a negative integer.
    *
    * @param that Other [[Region]]
    * @return Negative integer if this is less than other, zero if neither is greater, positive integer if this is greater than other
    */
  override def compare(that: Region): Int = that match {
    case Empty => -1 * (Empty compare this)
    case b: Block => Region.compare(this, b)
    case bs: BlockSet => Region.compare(this, bs)
  }

  override def relativePos(chrPos: Int): Option[Int] = orientation match {
    case _ if orientation != Plus && orientation != Minus =>
      throw new IllegalArgumentException("Orientation must be positive or negative")
    case Plus if chrPos >= start && chrPos < end => Some(chrPos - start)
    case Minus if chrPos >= start && chrPos < end => Some(end - 1 - chrPos)
    case _ => None
  }

  override def chrPos(relativePos: Int): Int = {
    if(relativePos < 0 || relativePos >= size) throw new IllegalArgumentException("Relative position must be between 0 and region size")
    else orientation match {
      case Plus => start + relativePos
      case Minus => end - 1 - relativePos
      case _ => throw new IllegalArgumentException("Orientation must be positive or negative")
    }
  }
}

/** Companion functions for [[Block]] */
object Block {

  /** An implicit ordering on [[Block]]s. Forwards to [[Region.compare]]. */
  implicit val ord = new Ordering[Block] {
    def compare(b1: Block, b2: Block): Int = b1.compare(b2)
  }
}


/** A collection of non-overlapping [[Block]]s on the same chromosome with the same [[Orientation]]
  *
  * A [[BlockSet]] can represent the structure of, e.g., a spliced RNA transcript.
  *
  * A [[BlockSet]] must have at least two [[Block]]s. For [[Region]]s with one [[Block]], use a [[Block]] instead.
  * The list of [[Block]]s passed to the constructor must be non-overlapping and on the same chromosome, and
  * have the same [[Orientation]]. They must be in ascending order of chromosome position.
  *
  * @param blocks Nonempty list of [[Block]]s
  */
final case class BlockSet(blocks: List[Block]) extends Region {

  override val end: Int = validateAndGetEnd()

  private def validateAndGetEnd(): Int = {
    if(blocks.length < 2) throw new IllegalArgumentException(s"Block set must have at least two blocks: ${blocks.mkString(", ")}")
    var lastEnd : Int = -1
    for(blk <- blocks) {
      if(blk.chr != chr) throw new IllegalArgumentException("All blocks must have same chromosome")
      if(blk.orientation != orientation) throw new IllegalArgumentException("All blocks must have same orientation")
      if(blk.start <= lastEnd) throw new IllegalArgumentException("Blocks must be non-overlapping and in order:\n" +
        blocks.mkString("\n"))
      lastEnd = blk.end
    }
    lastEnd
  }

  override def overlaps(feat: Region): Boolean = feat match {
    case Empty => false
    case b: Block => blocks.foldLeft(false)((bool, blk) => bool || b.overlaps(blk))
    case BlockSet(bs) => bs.foldLeft(false)((bool, blk) => bool || blk.overlaps(this))
  }

  override def overlapsCompatibleIntrons(o: Region): Boolean = o match {
    case Empty => false
    case b: Block => overlaps(b) && !getIntrons.foldLeft(false)((bool, blk) => bool || b.overlaps(blk))
    case bs: BlockSet =>
      def fromList(blks: List[Block]): Region = {
        blks match {
          case Nil => Empty
          case blk :: Nil => blk
          case blk1 :: blk2 :: tail => BlockSet(blks)
        }
      }
      overlaps(bs) && !overlaps(fromList(bs.getIntrons)) && !fromList(getIntrons).overlaps(bs)
  }

  override def union(feat: Region): Region = feat match {
    case Empty => this
    case b: Block => Region.union(b, this)
    case BlockSet(bs) => bs.foldLeft[Region](this)((f, b) => b.union(f))
  }

  override def intersection(feat: Region): Region = feat match {
    case Empty => Empty
    case b: Block => Region.intersection(b, this)
    case BlockSet(bs) => bs.foldLeft[Region](Empty)((f, b) => f.union(b.intersection(this)))
  }

  override def minus(feat: Region): Region = feat match {
    case Empty => BlockSet(blocks.map(b => Block(chr, b.start, b.end, orientation)))
    case b: Block =>
      val newBlks = ListBuffer[Block]()
      blocks.foreach(blk => {
        val m = blk.minus(b)
        if(!m.isEmpty) newBlks ++= m.blocks
      })
      if(newBlks.isEmpty) Empty
      else {
        val bl : List[Block] = newBlks.toList
        if(bl.length == 1) bl.head
        else BlockSet(bl)
      }
    case bs: BlockSet => blocks.foldLeft[Region](Empty)((f, b) => f.union(b.minus(bs)))
  }

  override def isEmpty: Boolean = false

  override def chr: String = blocks.head.chr

  override def start: Int = blocks.head.start

  override def orientation: Orientation = blocks.head.orientation

  override def addBlock(block: Block): Region = union(block)

  override def contains(feat: Region): Boolean = feat match {
    case Empty => false
    case b: Block => blocks.exists(_.contains(b))
    case BlockSet(bs) => bs.forall(contains(_))
  }

  override def numBlocks: Int = blocks.length

  /** Returns a string representation of this [[BlockSet]]. */
  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    sb.append(blocks.mkString(", "))
    sb.append(']')
    sb.toString()
  }

  /** Returns the result of comparing this with another [[Region]].
    *
    * If other [[Region]] is [[Empty]], return a negative integer.
    *
    * If other [[Region]] is a [[Block]], return -1 times the reverse comparison as in [[Block.compare]].
    *
    * If other [[Region]] is a [[BlockSet]], first compare spans as if they were single blocks as in [[Block.compare]]. Next compare number
    * of blocks. If the [[Region]]s have the same number of blocks, compare first respective pair of different [[Block]]s starting from the
    * left as in [[Block.compare]]. Return the first non-zero comparison, or zero if all of these comparisons are zero.
    *
    * @param that Other [[Region]]
    * @return Negative integer if this is less than other, zero if neither is greater, positive integer if this is greater than other
    */
  override def compare(that: Region): Int = that match {
    case Empty => -1 * (Empty compare this)
    case b: Block => Region.compare(this, b)
    case bs: BlockSet => Region.compare(this, bs)
  }

  override def relativePos(chrPos: Int): Option[Int] = orientation match {
    case _ if orientation != Plus && orientation != Minus =>
      throw new IllegalArgumentException("Orientation must be positive or negative")
    case Plus if chrPos >= start && chrPos < end =>
      val lo: (Int, Boolean) = // lo = (Cumulative size, whether we've found a block that the chromosome position overlaps)
        blocks.foldLeft[(Int, Boolean)]((0, false))((ib, blk) => {
          if(chrPos >= blk.start && chrPos < blk.end) { // Chromosome position overlaps this block
            (ib._1 + chrPos - blk.start, true)
          }
          else { // Chromosome position does not overlap this block
            if(chrPos < blk.start) ib // We've passed the chromosome position already
            else (ib._1 + blk.size, ib._2) // We haven't encountered the chromosome position yet. Add the size of this block.
          }
        })
      if(lo._2) Some(lo._1) else None
    case Minus if chrPos >= start && chrPos < end =>
      val lo: (Int, Boolean) = // lo = (Cumulative size, whether we've found a block that the chromosome position overlaps)
        blocks.foldRight[(Int, Boolean)]((0, false))((blk, ib) => {
          if(chrPos >= blk.start && chrPos < blk.end) { // Chromosome position overlaps this block
            (ib._1 + blk.end - 1 - chrPos, true)
          }
          else { // Chromosome position does not overlap this block
            if(chrPos >= blk.end) ib // We've passed the chromosome position already
            else (ib._1 + blk.size, ib._2) // We haven't encountered the chromosome position yet. Add the size of this block.
          }
        })
      if(lo._2) Some(lo._1) else None
    case _ => None
  }

  override def chrPos(relativePos: Int): Int = {
    if(relativePos < 0 || relativePos >= size) throw new IllegalArgumentException("Relative position must be between 0 and region size")
    orientation match {
      case Plus =>
        val firstBlockSize = blocks.head.size
        if(firstBlockSize > relativePos) start + relativePos // The relative position is within the first block
        else {
          if(blocks.size > 2) BlockSet(blocks.tail).chrPos(relativePos - firstBlockSize)
          else blocks.tail.head.chrPos(relativePos - firstBlockSize)
        } // Call recursively on tail of block list and add size of first block
      case Minus =>
        val lastBlock = blocks.last
        val lastBlockSize = lastBlock.size
        if(lastBlockSize > relativePos) lastBlock.end - 1 - relativePos // The relative position is within the last block
        else {
          if(blocks.size > 2) BlockSet(blocks.init).chrPos(relativePos - lastBlockSize)
          else blocks.head.chrPos(relativePos - lastBlockSize)
        } // Call recursively on init of block list and add size of first block
      case _ => throw new IllegalArgumentException("Orientation must be positive or negative")
    }
  }
}