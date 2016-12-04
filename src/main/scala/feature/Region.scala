package feature

import scala.collection.mutable.ListBuffer

/**
  * Created by prussell on 8/29/16.
  * A genomic annotation consisting of blocks
  */
sealed abstract class Region extends Ordered[Region] {

  /**
    * Test whether this [[Region]] overlaps another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the [[Region]]s overlap
    */
  def overlaps(feat: Region): Boolean

  /**
    * Test whether this [[Region]] contains another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the other [[Region]] is contained in this [[Region]]
    */
  def contains(feat: Region): Boolean

  /**
    * Create a new [[Region]] by taking the union of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return [[Region]] representing the union of the blocks of the two [[Region]]s
    */
  def union(feat: Region): Region

  /**
    * Create a new [[Region]] by taking the intersection of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return [[Region]] representing the intersection of the blocks of the two [[Region]]s
    */
  def intersection(feat: Region): Region

  /**
    * Create a new [[Region]] by removing positions covered by the blocks of this
    * and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return [[Region]] representing this [[Region]] minus the intersection with other [[Region]]
    */
  def minus(feat: Region): Region

  /**
    * Check if the [[Region]] is empty
    *
    * @return True iff the [[Region]] is empty
    */
  def isEmpty: Boolean

  /**
    * @return Chromosome name
    */
  def chr: String

  /**
    * @return Zero based start position (inclusive)
    */
  def start: Int

  /**
    * @return Zero based end position (exclusive)
    */
  def end: Int

  /**
    * @return [[Region]] orientation
    */
  def orientation: Orientation

  /**
    * Get the [[Block]]s of this [[Region]]
    *
    * @return List of [[Block]]s in order by coordinates
    */
  def blocks: List[Block]

  /**
    * Get the number of blocks
    * @return The number of blocks
    */
  def numBlocks: Int

  /**
    * Obtain a new [[Region]] by adding a [[Block]] to this [[Region]]
    *
    * @param block Block to add
    * @return New [[Region]] with [[Block]] added
    */
  def addBlock(block: Block): Region

  /**
    * Obtain a new [[Region]] by intersecting with an interval
    * @param newStart New start position
    * @param newEnd New end position
    * @return The region trimmed to the new start and end positions
    */
  def trim(newStart: Int, newEnd: Int): Region = {
    if(newStart > newEnd) throw new IllegalArgumentException("New start must be <= new end")
    if(newStart < 0) throw new IllegalArgumentException("New start must be >= 0")
    intersection(Block(chr, newStart, newEnd, orientation))
  }

  /**
    * Get the size of the region
    * @return The total length of the blocks
    */
  def size: Int = blocks.map(b => b.end - b.start).sum

  /**
    * Get the relative position within the region of a chromosome position
    * Region must have a defined orientation (positive or negative)
    * @param chrPos Chromosome position
    * @return Relative position within the region of the chromosome position,
    *         accounting for region orientation, or None if the chromosome position
    *         doesn't overlap the region
    */
  def relativePos(chrPos: Int): Option[Int]

  /**
    * Get the chromosome position corresponding to a relative position within the region
    * Region must have a defined orientation (positive or negative)
    * @param relativePos Relative position within the region, accounting for region orientation
    * @return Chromosome position corresponding to the relative position within the region,
    *         accounting for region orientation
    */
  def chrPos(relativePos: Int): Int

}

/**
  * Static methods
  */
object Region {

  /**
    * Take the union of two [[Block]]s
    *
    * @param b1   Block 1
    * @param b2   Block 2
    * @return The union of the two [[Block]]s as a new [[Region]]
    */
  def union(b1: Block, b2: Block): Region = {
    if(!Orientation.isCompatible(b1, b2)) throw new IllegalArgumentException("Orientations must be compatible")
    if(b1.chr != b2.chr) throw new IllegalArgumentException("Blocks must have same chromosome")
    val or = Orientation.consensus(b1.orientation, b2.orientation)
    if(b1.overlaps(b2) || b1.adjacent(b2)) {
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

  /**
    * Take the intersection of two [[Block]]s
    *
    * @param b1   Block 1
    * @param b2   Block 2
    * @return The intersection of the two [[Block]]s as a new [[Region]]
    */
  def intersection(b1: Block, b2: Block): Region = {
    if(b1.overlaps(b2)) {
      Block(b1.chr, scala.math.max(b1.start, b2.start), scala.math.min(b1.end, b2.end),
        Orientation.consensus(b1.orientation, b2.orientation))
    } else Empty
  }

  /**
    * Take the union of a [[Block]] and a [[BlockSet]]
    *
    * @param b    A [[Block]]
    * @param bs   A [[BlockSet]]
    * @return The union of the two [[Region]]s
    */
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

  /**
    * Take the intersection of a [[Block]] and a [[BlockSet]]
    *
    * @param b    A [[Block]]
    * @param bs   A [[BlockSet]]
    * @return The intersection of the two [[Region]]s
    */
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

  /**
    * Remove the intersection of two blocks from one of the blocks
    * @param b1   Block to subtract from
    * @param b2   Block to subtract
    * @return [[Region]] representing the first block minus the intersection with the second block
    */
  def minus(b1: Block, b2: Block): Region = {
    if(!Orientation.isCompatible(b1, b2)) b1
    else if(!b1.overlaps(b2)) b1
    else if(b2.contains(b1)) Empty
    else if(b1.contains(b2)) { // b1 contains b2
      if(b1.start == b2.start) { // b1 contains b2 and they have equal start positions
        if(b1.end == b2.end) { // b1 == b2
          Empty
        } else { // b1 contains b2, equal start positions, different end positions
          Block(b1.chr, b2.end, b1.end, b1.orientation)
        }
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

  /**
    * Compare two blocks
    * @param b1 Block 1
    * @param b2 Block 2
    * @return Positive integer if block 1 is greater; negative if block 2 is greater; zero if equal
    */
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

  /**
    * Compare block and block set
    * @param b Block
    * @param bs Block set
    * @return Positive integer if block is greater; negative if block set is greater
    */
  def compare(b: Block, bs: BlockSet): Int = {
    // First compare span
    val cb = compare(b, Block(bs.chr, bs.start, bs.end, bs.orientation))
    if(cb != 0) cb
    else -1 // If same span, block is less than block set
  }

  /**
    * Compare block set and block
    * @param bs Block set
    * @param b Block
    * @return Positive integer if block set is greater; negative if block is greater
    */
  def compare(bs: BlockSet, b: Block): Int = -1 * compare(b, bs)

  /**
    * Compare two block sets
    * @param bs1 Block set 1
    * @param bs2 Block set 2
    * @return Positive integer if block set 1 is greater; negative if block set 2 is greater; zero if equal
    */
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

/**
  * Empty [[Region]]
  */
case object Empty extends Region {

  /**
    * Test whether this [[Region]] overlaps another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the [[Region]]s overlap
    */
  override def overlaps(feat: Region): Boolean = false

  /**
    * Create a new [[Region]] by taking the union of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the union of the blocks of the two [[Region]]s
    */
  override def union(feat: Region): Region = feat match {
    case Empty => Empty
    case _ => feat
  }

  /**
    * Create a new [[Region]] by taking the intersection of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the intersection of the blocks of the two [[Region]]s
    */
  override def intersection(feat: Region): Region = Empty

  /**
    * Create a new [[Region]] by removing positions covered by the blocks of this
    * and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing this [[Region]] minus the intersection with other [[Region]]
    */
  override def minus(feat: Region): Region = Empty


  /**
    * Check if the [[Region]] is empty
    *
    * @return True iff the [[Region]] is empty
    */
  override def isEmpty: Boolean = true

  /**
    * @return Chromosome name
    */
  override def chr: String = throw new IllegalStateException("Empty region")

  /**
    * @return Zero based start position (inclusive)
    */
  override def start: Int = throw new IllegalStateException("Empty region")

  /**
    * @return Zero based end position (exclusive)
    */
  override def end: Int = throw new IllegalStateException("Empty region")

  /**
    * @return Region orientation
    */
  override def orientation: Orientation = throw new IllegalStateException("Empty region")

  /**
    * Get the [[Block]]s of this [[Region]]
    *
    * @return List of [[Block]]s in order by coordinates
    */
  override def blocks: List[Block] = Nil

  /**
    * Obtain a new [[Region]] by adding a [[Block]] to this [[Region]]
    *
    * @param block Block to add
    * @return New [[Region]] with [[Block]] added
    */
  override def addBlock(block: Block): Region = block

  /**
    * Test whether this [[Region]] contains another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the other [[Region]] is contained in this [[Region]]
    */
  override def contains(feat: Region): Boolean = false

  /**
    * Get the number of blocks
    *
    * @return The number of blocks
    */
  override val numBlocks: Int = 0

  override def trim(newStart: Int, newEnd: Int): Region = Empty

  override def toString: String = "Empty"

  override def compare(that: Region): Int = that match {
    case Empty => 0
    case _ => 1
  }

  /**
    * Get the relative position within the region of a genomic position
    * Region must have a defined orientation (positive or negative)
    *
    * @param chrPos Chromosome position
    * @return Relative position within the region of the chromosome position,
    *         accounting for region orientation, or None if the chromosome position
    *         doesn't overlap the region
    */
  override def relativePos(chrPos: Int): Option[Int] = None

  /**
    * Get the chromosome position corresponding to a relative position within the region
    * Region must have a defined orientation (positive or negative)
    *
    * @param relativePos Relative position within the region, accounting for region orientation
    * @return Chromosome position corresponding to the relative position within the region,
    *         accounting for region orientation
    */
  override def chrPos(relativePos: Int): Int = throw new IllegalStateException("Empty region")
}

/**
  * A single contiguous block
  * @param chr Chromosome name
  * @param start Zero based start position (inclusive)
  * @param end Zero based end position (exclusive)
  * @param orientation Orientation
  */
final case class Block(chr: String, start: Int, end: Int, orientation: Orientation) extends Region {

  if(start < 0) throw new IllegalArgumentException("Start must be nonnegative")
  if(end <= start) throw new IllegalArgumentException("End must be greater than start. Otherwise use empty region")
  if(chr.isEmpty) throw new IllegalArgumentException("Chromosome name must not be empty")

  /**
    * Test whether this [[Region]] overlaps another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the [[Region]]s overlap
    */
  override def overlaps(feat: Region): Boolean = feat match {
    case Empty => false
    case Block(c, s, e, o) =>
      if(chr != c) false
      else if(!Orientation.isCompatible(orientation, o)) false
      else (start < e && e <= end) || (s < end && end <= e)
    case BlockSet(bs) =>
      if(chr != feat.chr) false
      else if(!Orientation.isCompatible(orientation, feat.orientation)) false
      else bs.foldLeft(false)((c: Boolean, b: Block) => c || b.overlaps(this)) // Check for overlap of any block
  }

  /**
    * Create a new [[Region]] by taking the union of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the union of the blocks of the two [[Region]]s
    */
  override def union(feat: Region): Region = feat match {
    case Empty => this
    case b: Block => Region.union(this, b)
    case bs: BlockSet => Region.union(this, bs)
  }

  /**
    * Create a new [[Region]] by taking the intersection of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the intersection of the blocks of the two [[Region]]s
    */
  override def intersection(feat: Region): Region = feat match {
    case Empty => Empty
    case b: Block => Region.intersection(this, b)
    case bs: BlockSet => Region.intersection(this, bs)
  }

  /**
    * Create a new [[Region]] by removing positions covered by the blocks of this
    * and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing this [[Region]] minus the intersection with other [[Region]]
    */
  override def minus(feat: Region): Region = {
    if(feat.isEmpty) this
    else if(!Orientation.isCompatible(orientation, feat.orientation)) this
    else feat match {
      case Empty => this
      case b: Block => Region.minus(this, b)
      case BlockSet(bs) => bs.foldLeft[Region](this)((f, b) => f.minus(b))
    }
  }

  /**
    * Check if the [[Region]] is empty
    *
    * @return True iff the [[Region]] is empty
    */
  override def isEmpty: Boolean = false

  /**
    * Get the [[Block]]s of this [[Region]]
    *
    * @return List of [[Block]]s in order by coordinates
    */
  override def blocks: List[Block] = List(this)

  /**
    * Obtain a new [[Region]] by adding a [[Block]] to this [[Region]]
    *
    * @param block Block to add
    * @return New [[Region]] with [[Block]] added
    */
  override def addBlock(block: Block): Region = Region.union(this, block)

  /**
    * Get a copy of this block with the orientation changed
    * @param or New orientation
    * @return Copy of this block with the orientation changed
    */
  def changeOrientation(or: Orientation): Block = Block(chr, start, end, or)

  /**
    * Test whether this [[Region]] contains another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the other [[Region]] is contained in this [[Region]]
    */
  override def contains(feat: Region): Boolean = feat match {
    case Empty => false
    case f: Region => Orientation.isCompatible(this, f) &&
      chr == f.chr &&
      f.start >= start &&
      f.end <= end
  }

  /**
    * Get the number of blocks
    *
    * @return The number of blocks
    */
  override def numBlocks: Int = 1

  /**
    * Check whether blocks are directly adjacent but not overlapping
    * Ignores orientation
    * @param block Other block
    * @return True iff there is no gap between the blocks and they don't overlap
    */
  def adjacent(block: Block): Boolean = {
    if(chr != block.chr) false
    else start == block.end || end == block.start
  }

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

  override def compare(that: Region): Int = that match {
    case Empty => -1 * (Empty compare this)
    case b: Block => Region.compare(this, b)
    case bs: BlockSet => Region.compare(this, bs)
  }

  /**
    * Get the relative position within the region of a genomic position
    * Region must have a defined orientation (positive or negative)
    *
    * @param chrPos Chromosome position
    * @return Relative position within the region of the chromosome position,
    *         accounting for region orientation, or None if the chromosome position
    *         doesn't overlap the region
    */
  override def relativePos(chrPos: Int): Option[Int] = orientation match {
    case _ if orientation != Plus && orientation != Minus =>
      throw new IllegalArgumentException("Orientation must be positive or negative")
    case Plus if chrPos >= start && chrPos < end => Some(chrPos - start)
    case Minus if chrPos >= start && chrPos < end => Some(end - 1 - chrPos)
    case _ => None
  }

  /**
    * Get the chromosome position corresponding to a relative position within the region
    * Region must have a defined orientation (positive or negative)
    *
    * @param relativePos Relative position within the region, accounting for region orientation
    * @return Chromosome position corresponding to the relative position within the region,
    *         accounting for region orientation
    */
  override def chrPos(relativePos: Int): Int = {
    if(relativePos < 0 || relativePos >= size) throw new IllegalArgumentException("Relative position must be between 0 and region size")
    else orientation match {
      case Plus => start + relativePos
      case Minus => end - 1 - relativePos
      case _ => throw new IllegalArgumentException("Orientation must be positive or negative")
    }
  }
}

/**
  * A region consisting of multiple blocks
  * @param blocks The blocks
  */
final case class BlockSet(blocks: List[Block]) extends Region {

  override val end = validateAndGetEnd()

  /**
    * Check that the blocks are valid and return the end coordinate of the last block
    * @return End coordinate of last block
    */
  private def validateAndGetEnd(): Int = {
    if(blocks.length < 2) throw new IllegalArgumentException(s"Block set must have at least two blocks: ${blocks.mkString(", ")}")
    var lastEnd : Int = -1
    for(blk <- blocks) {
      if(blk.chr != chr) throw new IllegalArgumentException("All blocks must have same chromosome")
      if(blk.orientation != orientation) throw new IllegalArgumentException("All blocks must have same orientation")
      if(blk.start <= lastEnd) throw new IllegalArgumentException("Blocks must be non-overlapping and in order:" +
        blocks.mkString(", "))
      lastEnd = blk.end
    }
    if(lastEnd < 0) throw new IllegalArgumentException("Block list must be nonempty")
    lastEnd
  }

  /**
    * Test whether this [[Region]] overlaps another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the [[Region]]s overlap
    */
  override def overlaps(feat: Region): Boolean = feat match {
    case Empty => false
    case b: Block => blocks.foldLeft(false)((bool, blk) => bool || b.overlaps(blk))
    case BlockSet(bs) => bs.foldLeft(false)((bool, blk) => bool || blk.overlaps(this))
  }

  /**
    * Create a new [[Region]] by taking the union of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the union of the blocks of the two [[Region]]s
    */
  override def union(feat: Region): Region = feat match {
    case Empty => this
    case b: Block => Region.union(b, this)
    case BlockSet(bs) => bs.foldLeft[Region](this)((f, b) => b.union(f))
  }

  /**
    * Create a new [[Region]] by taking the intersection of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the intersection of the blocks of the two [[Region]]s
    */
  override def intersection(feat: Region): Region = feat match {
    case Empty => Empty
    case b: Block => Region.intersection(b, this)
    case BlockSet(bs) => bs.foldLeft[Region](Empty)((f, b) => f.union(b.intersection(this)))
  }

  /**
    * Create a new [[Region]] by removing positions covered by the blocks of this
    * and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing this [[Region]] minus the intersection with other [[Region]]
    */
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

  /**
    * Check if the [[Region]] is empty
    *
    * @return True iff the [[Region]] is empty
    */
  override def isEmpty: Boolean = false

  /**
    * @return Chromosome name
    */
  override def chr: String = blocks.head.chr

  /**
    * @return Zero based start position (inclusive)
    */
  override def start: Int = blocks.head.start

  /**
    * @return Region orientation
    */
  override def orientation: Orientation = blocks.head.orientation

  /**
    * Obtain a new [[Region]] by adding a [[Block]] to this [[Region]]
    *
    * @param block Block to add
    * @return New [[Region]] with [[Block]] added
    */
  override def addBlock(block: Block): Region = union(block)

  /**
    * Test whether this [[Region]] contains another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the other [[Region]] is contained in this [[Region]]
    */
  override def contains(feat: Region): Boolean = feat match {
    case Empty => false
    case b: Block => blocks.exists(_.contains(b))
    case BlockSet(bs) => bs.forall(contains(_))
  }

  /**
    * Get the number of blocks
    *
    * @return The number of blocks
    */
  override def numBlocks: Int = blocks.length

  override def toString: String = {
    val sb = new StringBuilder
    sb.append('[')
    sb.append(blocks.mkString(", "))
    sb.append(']')
    sb.toString()
  }

  override def compare(that: Region): Int = that match {
    case Empty => -1 * (Empty compare this)
    case b: Block => Region.compare(this, b)
    case bs: BlockSet => Region.compare(this, bs)
  }

  /**
    * Get the relative position within the region of a genomic position
    * Region must have a defined orientation (positive or negative)
    *
    * @param chrPos Chromosome position
    * @return Relative position within the region of the chromosome position,
    *         accounting for region orientation, or None if the chromosome position
    *         doesn't overlap the region
    */
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

  /**
    * Get the chromosome position corresponding to a relative position within the region
    * Region must have a defined orientation (positive or negative)
    *
    * @param relativePos Relative position within the region, accounting for region orientation
    * @return Chromosome position corresponding to the relative position within the region,
    *         accounting for region orientation
    */
  override def chrPos(relativePos: Int): Int = {
    if(relativePos < 0 || relativePos >= size) throw new IllegalArgumentException("Relative position must be between 0 and region size")
    if(blocks.size < 2) throw new IllegalStateException("There are less than 2 blocks")
    else orientation match {
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