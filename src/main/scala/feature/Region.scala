package feature

import scala.collection.mutable.ListBuffer

/**
  * Created by prussell on 8/29/16.
  * A genomic annotation consisting of blocks
  */
sealed abstract class Region {

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
 *
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
  override def union(feat: Region): Region = {
    feat match {
      case Empty => Empty
      case _ => feat
    }
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
  override def numBlocks: Int = 0

  override def toString: String = "Empty"

}

/**
  * A single contiguous block
  * @param chr Chromosome name
  * @param start Zero based start position (inclusive)
  * @param end Zero based end position (exclusive)
  * @param orientation Orientation
  */
case class Block(chr: String, start: Int, end: Int, orientation: Orientation) extends Region {

  if(start < 0) throw new IllegalArgumentException("Start must be nonnegative")
  if(end <= start) throw new IllegalArgumentException("End must be greater than start. Otherwise use empty region")
  if(chr.isEmpty) throw new IllegalArgumentException("Chromosome name must not be empty")

  /**
    * Test whether this [[Region]] overlaps another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return True iff the [[Region]]s overlap
    */
  override def overlaps(feat: Region): Boolean = {
    feat match {
      case Empty => false
      case Block(c, s, e, o) =>
        if(chr != c) false
        else if(!Orientation.isCompatible(orientation, o)) false
        else (start < e && e <= end) || (s < end && end <= e)
      case BlockSet(bs) =>
        if(chr != feat.chr) false
        else if(!Orientation.isCompatible(orientation, feat.orientation)) false
        else bs.foldLeft(false)((c: Boolean, b: Block) => c || b.overlaps(this)) // Check for overlap of any block
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Region]] by taking the union of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the union of the blocks of the two [[Region]]s
    */
  override def union(feat: Region): Region = {
    feat match {
      case Empty => this
      case b: Block => Region.union(this, b)
      case bs: BlockSet => Region.union(this, bs)
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Region]] by taking the intersection of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the intersection of the blocks of the two [[Region]]s
    */
  override def intersection(feat: Region): Region = {
    feat match {
      case Empty => Empty
      case b: Block => Region.intersection(this, b)
      case bs: BlockSet => Region.intersection(this, bs)
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
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
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
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
  override def contains(feat: Region): Boolean = {
    feat match {
      case Empty => false
      case f: Region => Orientation.isCompatible(this, f) &&
        chr == f.chr &&
        f.start >= start &&
        f.end <= end
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
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

}

/**
  * A region consisting of multiple blocks
  * @param blocks The blocks
  */
case class BlockSet(blocks: List[Block]) extends Region {

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
  override def overlaps(feat: Region): Boolean = {
    feat match {
      case Empty => false
      case b: Block => blocks.foldLeft(false)((bool, blk) => bool || b.overlaps(blk))
      case BlockSet(bs) => bs.foldLeft(false)((bool, blk) => bool || blk.overlaps(this))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Region]] by taking the union of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the union of the blocks of the two [[Region]]s
    */
  override def union(feat: Region): Region = {
    feat match {
      case Empty => this
      case b: Block => Region.union(b, this)
      case BlockSet(bs) => bs.foldLeft[Region](this)((f, b) => b.union(f))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Region]] by taking the intersection of positions covered by the blocks
    * of this and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing the intersection of the blocks of the two [[Region]]s
    */
  override def intersection(feat: Region): Region = {
    feat match {
      case Empty => Empty
      case b: Block => Region.intersection(b, this)
      case BlockSet(bs) => bs.foldLeft[Region](Empty)((f, b) => f.union(b.intersection(this)))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Region]] by removing positions covered by the blocks of this
    * and another [[Region]]
    *
    * @param feat Other [[Region]]
    * @return Region representing this [[Region]] minus the intersection with other [[Region]]
    */
  override def minus(feat: Region): Region = {
    feat match {
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
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
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
  override def contains(feat: Region): Boolean = {
    feat match {
      case Empty => false
      case b: Block => blocks.exists(_.contains(b))
      case BlockSet(bs) => bs.forall(contains(_))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
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

}