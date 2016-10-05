package feature

import scala.collection.mutable.ListBuffer

/**
  * Created by prussell on 8/29/16.
  * A genomic annotation consisting of blocks
  */
sealed abstract class Feature {

  /**
    * Test whether this [[Feature]] overlaps another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s overlap
    */
  def overlaps(feat: Feature): Boolean

  /**
    * Test whether this [[Feature]] contains another [[Feature]]
    * @param feat Other [[Feature]]
    * @return True iff the other [[Feature]] is contained in this [[Feature]]
    */
  def contains(feat: Feature): Boolean

  /**
    * Create a new [[Feature]] by taking the union of positions covered by the blocks
    * of this and another [[Feature]]
    * Returned feature will have block names removed
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return [[Feature]] representing the union of the blocks of the two [[Feature]]s
    */
  def union(feat: Feature, name: String): Feature

  /**
    * Create a new [[Feature]] by taking the intersection of positions covered by the blocks
    * of this and another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return [[Feature]] representing the intersection of the blocks of the two [[Feature]]s
    */
  def intersection(feat: Feature, name: String): Feature

  /**
    * Create a new [[Feature]] by removing positions covered by the blocks of this
    * and another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return [[Feature]] representing this [[Feature]] minus the intersection with other [[Feature]]
    */
  def minus(feat: Feature, name: String): Feature

  /**
    * Get a copy of this [[Feature]] with a new name
    * @param name New name
    * @return New [[Feature]] with same blocks and new name
    */
  def rename(name: String): Feature

  /**
    * Check whether two [[Feature]]s are equal except for their names
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s are equal except for their names
    */
  def equalsIgnoreName(feat: Feature): Boolean

  /**
    * Check if the [[Feature]] is empty
    *
    * @return True iff the [[Feature]] is empty
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
    * @return [[Feature]] orientation
    */
  def orientation: Orientation

  /**
    * @return Feature name
    */
  def name: String

  /**
    * Get the [[Block]]s of this [[Feature]]
    * @return List of [[Block]]s in order by coordinates
    */
  def blocks: List[Block]

  /**
    * Get the number of blocks
    * @return The number of blocks
    */
  def numBlocks: Int

  /**
    * Obtain a new [[Feature]] by adding a [[Block]] to this [[Feature]]
    *
    * @param block Block to add
    * @param name Name for returned [[Feature]] if different from this
    * @return New [[Feature]] with [[Block]] added
    */
  def addBlock(block: Block, name: String): Feature

}

/**
  * Static methods
  */
object Feature {

  /**
    * Take the union of two [[Block]]s
    * @param b1 Block 1
    * @param b2 Block 2
    * @param name Name for returned [[Feature]]
    * @return The union of the two [[Block]]s as a new [[Feature]]
    */
  def union(b1: Block, b2: Block, name: String): Feature = {
    if(!Orientation.isCompatible(b1, b2)) throw new IllegalArgumentException("Orientations must be compatible")
    if(b1.chr != b2.chr) throw new IllegalArgumentException("Blocks must have same chromosome")
    val or = Orientation.consensus(b1.orientation, b2.orientation)
    if(b1.overlaps(b2) || b1.adjacent(b2)) {
      Block(b1.chr, scala.math.min(b1.start, b2.start), scala.math.max(b1.end, b2.end), or, name)
    }
    else {
      if(b1.start < b2.start) {
        BlockSet(List(
          Block(b1.chr, b1.start, b1.end, or, ""),
          Block(b2.chr, b2.start, b2.end, or, "")
        ), name)
      } else {
        BlockSet(List(
          Block(b2.chr, b2.start, b2.end, or, ""),
          Block(b1.chr, b1.start, b1.end, or, "")
        ), name)
      }
    }
  }

  /**
    * Take the intersection of two [[Block]]s
    * @param b1 Block 1
    * @param b2 Block 2
    * @param name Name for returned [[Feature]] if nonempty
    * @return The intersection of the two [[Block]]s as a new [[Feature]]
    */
  def intersection(b1: Block, b2: Block, name: String): Feature = {
    if(b1.overlaps(b2)) {
      Block(b1.chr, scala.math.max(b1.start, b2.start), scala.math.min(b1.end, b2.end),
        Orientation.consensus(b1.orientation, b2.orientation), name)
    } else Empty
  }

  /**
    * Take the union of a [[Block]] and a [[BlockSet]]
    * @param b A [[Block]]
    * @param bs A [[BlockSet]]
    * @param name Name for returned [[Feature]]
    * @return The union of the two [[Feature]]s
    */
  def union(b: Block, bs: BlockSet, name: String): Feature = {

    // Check for compatible orientations and chromosomes, then save consensus
    if(!Orientation.isCompatible(b, bs)) throw new IllegalArgumentException("Incompatible orientations")
    if(b.chr != bs.chr) throw new IllegalArgumentException("Different chromosomes")
    val strand = Orientation.consensus(b.orientation, bs.orientation)
    val chr: String = b.chr

    val it: Iterator[Block] = bs.blocks.iterator
    val rtrnBlks: collection.mutable.MutableList[Block] = collection.mutable.MutableList.empty // Blocks of the returned feature
    var added, overlap: Boolean = false
    var overlapStart, overlapEnd: Int = -1
    while(it.hasNext) {
      val blk = it.next()
      if(overlap) { // Already in an overlap
        if(b.overlaps(blk) || b.end == blk.start) {
          overlapEnd = math.max(overlapEnd, blk.end)
        } else {
          rtrnBlks += Block(chr, overlapStart, overlapEnd, strand, "")
          rtrnBlks += Block(chr, blk.start, blk.end, strand, "")
          overlap = false
          added = true
        }
      } else { // Not already  in an overlap
        if(b.end < blk.start) { // b is entirely before blk
          if(!added) rtrnBlks += Block(chr, b.start, b.end, strand, "")
          rtrnBlks += Block(chr, blk.start, blk.end, strand, "")
          added = true
        } else if(b.start > blk.end) { // b is entirely after blk
          rtrnBlks += Block(chr, blk.start, blk.end, strand, "")
        } else { // New overlap
          overlap = true
          overlapStart = math.min(b.start, blk.start)
          overlapEnd = math.max(b.end, blk.end)
        }
      }
    }
    if(overlap) {
      rtrnBlks += Block(chr, overlapStart, overlapEnd, strand, "")
      overlap = false
      added = true
    }
    if(!added) rtrnBlks += Block(chr, b.start, b.end, strand, "")
    if(rtrnBlks.length == 1) rtrnBlks.head.rename(name)
    else BlockSet(rtrnBlks.toList, name)
  }

  /**
    * Take the intersection of a [[Block]] and a [[BlockSet]]
    * @param b A [[Block]]
    * @param bs A [[BlockSet]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return The intersection of the two [[Feature]]s
    */
  def intersection(b: Block, bs: BlockSet, name: String): Feature = {
    ???
  }

  /**
    * Remove the intersection of two blocks from one of the blocks
    * @param b1 Block to subtract from
    * @param b2 Block to subtract
    * @param name Name for returned [[Feature]] if different from original block
    * @return [[Feature]] representing the first block minus the intersection with the second block
    */
  def minus(b1: Block, b2: Block, name: String): Feature = {
    if(!Orientation.isCompatible(b1, b2)) b1
    else if(!b1.overlaps(b2)) b1.rename(name)
    else if(b2.contains(b1)) Empty
    else if(b1.contains(b2)) { // b1 contains b2
      if(b1.start == b2.start) { // b1 contains b2 and they have equal start positions
        if(b1.end == b2.end) { // b1 == b2
          Empty
        } else { // b1 contains b2, equal start positions, different end positions
          Block(b1.chr, b2.end, b1.end, b1.orientation, name)
        }
      } else { // b1 contains b2 and they have different start positions
        if(b1.end == b2.end) { // b1 contains b2, different start positions, same end position
          Block(b1.chr, b1.start, b2.start, b1.orientation, name)
        } else { // b1 contains b2, different start and end positions
          BlockSet(List(Block(b1.chr, b1.start, b2.start, b1.orientation, ""),
            Block(b1.chr, b2.end, b1.end, b1.orientation, "")), name)
        }
      }
    } else { // b1 and b2 overlap, neither is contained in the other
      if(b1.start < b2.start) Block(b1.chr, b1.start, b2.start, b1.orientation, name)
      else Block(b1.chr, b2.end, b1.end, b1.orientation, name)
    }
  }

}

/**
  * Empty [[Feature]]
  */
case object Empty extends Feature {

  /**
    * Test whether this [[Feature]] overlaps another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s overlap
    */
  override def overlaps(feat: Feature): Boolean = false

  /**
    * Create a new [[Feature]] by taking the union of positions covered by the blocks
    * of this and another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing the union of the blocks of the two [[Feature]]s
    */
  override def union(feat: Feature, name: String): Feature = {
    feat match {
      case Empty => Empty
      case _ => feat.rename(name)
    }
  }

  /**
    * Create a new [[Feature]] by taking the intersection of positions covered by the blocks
    * of this and another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing the intersection of the blocks of the two [[Feature]]s
    */
  override def intersection(feat: Feature, name: String): Feature = Empty

  /**
    * Create a new [[Feature]] by removing positions covered by the blocks of this
    * and another [[Feature]]
    * Returned feature will have block names removed
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing this [[Feature]] minus the intersection with other [[Feature]]
    */
  override def minus(feat: Feature, name: String): Feature = Empty


  /**
    * Check if the [[Feature]] is empty
    *
    * @return True iff the [[Feature]] is empty
    */
  override def isEmpty: Boolean = true

  /**
    * @return Chromosome name
    */
  override def chr: String = throw new IllegalStateException("Empty feature")

  /**
    * @return Zero based start position (inclusive)
    */
  override def start: Int = throw new IllegalStateException("Empty feature")

  /**
    * @return Zero based end position (exclusive)
    */
  override def end: Int = throw new IllegalStateException("Empty feature")

  /**
    * @return Feature orientation
    */
  override def orientation: Orientation = throw new IllegalStateException("Empty feature")

  /**
    * @return Feature name
    */
  override def name: String = throw new IllegalStateException("Empty feature")

  /**
    * Get the [[Block]]s of this [[Feature]]
    *
    * @return List of [[Block]]s in order by coordinates
    */
  override def blocks: List[Block] = Nil

  /**
    * Obtain a new [[Feature]] by adding a [[Block]] to this [[Feature]]
    *
    * @param block Block to add
    * @param name Name for returned [[Feature]] if different from this
    * @return New [[Feature]] with [[Block]] added
    */
  override def addBlock(block: Block, name: String): Feature = block.rename(name)

  /**
    * Get a copy of this [[Feature]] with a new name
    *
    * @param name New name
    * @return New [[Feature]] with same blocks and new name
    */
  override def rename(name: String): Feature = throw new IllegalStateException("Empty feature")

  /**
    * Check whether two [[Feature]]s are equal except for their names
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s are equal except for their names
    */
  override def equalsIgnoreName(feat: Feature): Boolean = {
    feat match {
      case Empty => true
      case _ => false
    }
  }

  /**
    * Test whether this [[Feature]] contains another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the other [[Feature]] is contained in this [[Feature]]
    */
  override def contains(feat: Feature): Boolean = false

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
  * @param name Feature name
  */
case class Block(chr: String, start: Int, end: Int, orientation: Orientation, name: String) extends Feature {

  if(start < 0) throw new IllegalArgumentException("Start must be nonnegative")
  if(end <= start) throw new IllegalArgumentException("End must be greater than start. Otherwise use empty feature")
  if(chr.isEmpty) throw new IllegalArgumentException("Chromosome name must not be empty")

  /**
    * Test whether this [[Feature]] overlaps another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s overlap
    */
  override def overlaps(feat: Feature): Boolean = {
    feat match {
      case Empty => false
      case Block(c, s, e, o, n) =>
        if(chr != c) false
        else if(!Orientation.isCompatible(orientation, o)) false
        else (start < e && e <= end) || (s < end && end <= e)
      case BlockSet(bs, n) =>
        if(chr != feat.chr) false
        else if(!Orientation.isCompatible(orientation, feat.orientation)) false
        else bs.foldLeft(false)((c: Boolean, b: Block) => c || b.overlaps(this)) // Check for overlap of any block
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Feature]] by taking the union of positions covered by the blocks
    * of this and another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing the union of the blocks of the two [[Feature]]s
    */
  override def union(feat: Feature, name: String): Feature = {
    feat match {
      case Empty => this.rename(name)
      case b: Block => Feature.union(this, b, name)
      case bs: BlockSet => Feature.union(this, bs, name)
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Feature]] by taking the intersection of positions covered by the blocks
    * of this and another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing the intersection of the blocks of the two [[Feature]]s
    */
  override def intersection(feat: Feature, name: String): Feature = {
    feat match {
      case Empty => Empty
      case b: Block => Feature.intersection(this, b, name)
      case bs: BlockSet => Feature.intersection(this, bs, name)
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Feature]] by removing positions covered by the blocks of this
    * and another [[Feature]]
    * Returned feature will have block names removed
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing this [[Feature]] minus the intersection with other [[Feature]]
    */
  override def minus(feat: Feature, name: String): Feature = {
    if(feat.isEmpty) this.rename(name)
    else if(!Orientation.isCompatible(orientation, feat.orientation)) this.rename(name)
    else feat match {
      case Empty => this.rename(name)
      case b: Block => Feature.minus(this, b, name)
      case BlockSet(bs, n) => bs.foldLeft[Feature](this)((f, b) => f.minus(b, name))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Check if the [[Feature]] is empty
    *
    * @return True iff the [[Feature]] is empty
    */
  override def isEmpty: Boolean = false

  /**
    * Get the [[Block]]s of this [[Feature]]
    *
    * @return List of [[Block]]s in order by coordinates
    */
  override def blocks: List[Block] = List(this)

  /**
    * Obtain a new [[Feature]] by adding a [[Block]] to this [[Feature]]
    *
    * @param block Block to add
    * @param name Name for returned [[Feature]] if different from this
    * @return New [[Feature]] with [[Block]] added
    */
  override def addBlock(block: Block, name: String): Feature = Feature.union(this, block, name)

  /**
    * Get a copy of this [[Feature]] with a new name
    *
    * @param newName New name
    * @return New [[Feature]] with same blocks and new name
    */
  override def rename(newName: String): Feature = Block(chr, start, end, orientation, newName)

  /**
    * Get a copy of this block with the orientation changed
    * @param or New orientation
    * @return Copy of this block with the orientation changed
    */
  def changeOrientation(or: Orientation): Block = Block(chr, start, end, or, name)

  /**
    * Check whether two [[Feature]]s are equal except for their names
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s are equal except for their names
    */
  override def equalsIgnoreName(feat: Feature): Boolean = {
    feat match {
      case Block(this.chr, this.start, this.end, this.orientation, nm) => true
      case _ => false
    }
  }

  /**
    * Test whether this [[Feature]] contains another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the other [[Feature]] is contained in this [[Feature]]
    */
  override def contains(feat: Feature): Boolean = {
    feat match {
      case Empty => false
      case f: Feature => Orientation.isCompatible(this, f) &&
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
    sb.append(name)
    sb.append(':')
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
  * A feature consisting of multiple blocks
  * @param blocks The blocks
  * @param name Feature name
  */
case class BlockSet(blocks: List[Block], name: String) extends Feature {

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
    * Test whether this [[Feature]] overlaps another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s overlap
    */
  override def overlaps(feat: Feature): Boolean = {
    feat match {
      case Empty => false
      case b: Block => blocks.foldLeft(false)((bool, blk) => bool || b.overlaps(blk))
      case BlockSet(bs, n) => bs.foldLeft(false)((bool, blk) => bool || blk.overlaps(this))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Feature]] by taking the union of positions covered by the blocks
    * of this and another [[Feature]]
    * Returned feature will have block names removed
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing the union of the blocks of the two [[Feature]]s
    */
  override def union(feat: Feature, name: String): Feature = {
    feat match {
      case Empty => this.rename(name)
      case b: Block => Feature.union(b, this, name)
      case BlockSet(bs, n) => bs.foldLeft[Feature](this)((f, b) => b.union(f, name))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Feature]] by taking the intersection of positions covered by the blocks
    * of this and another [[Feature]]
    * Returned feature will have block names removed
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing the intersection of the blocks of the two [[Feature]]s
    */
  override def intersection(feat: Feature, name: String): Feature = {
    feat match {
      case Empty => Empty
      case b: Block => Feature.intersection(b, this, name)
      case BlockSet(bs, n) => bs.foldLeft[Feature](Empty)((f, b) => f.union(b.intersection(this, name), name))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Create a new [[Feature]] by removing positions covered by the blocks of this
    * and another [[Feature]]
    * Returned feature will have block names removed
    *
    * @param feat Other [[Feature]]
    * @param name Name for returned [[Feature]] if nonempty
    * @return Feature representing this [[Feature]] minus the intersection with other [[Feature]]
    */
  override def minus(feat: Feature, name: String): Feature = {
    feat match {
      case Empty => BlockSet(blocks.map(b => Block(chr, b.start, b.end, orientation, "")), name)
      case b: Block =>
        val newBlks = ListBuffer[Block]()
        blocks.foreach(blk => {
          val m = blk.minus(b, "")
          if(!m.isEmpty) newBlks ++= m.blocks
        })
        if(newBlks.isEmpty) Empty
        else {
          val bl : List[Block] = newBlks.toList
          if(bl.length == 1) bl.head.rename(name)
          else BlockSet(bl, name)
        }
      case bs: BlockSet => blocks.foldLeft[Feature](Empty)((f, b) => f.union(b.minus(bs, name), name))
      case _ => throw new IllegalArgumentException("Not implemented for " + feat.toString)
    }
  }

  /**
    * Check if the [[Feature]] is empty
    *
    * @return True iff the [[Feature]] is empty
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
    * @return Feature orientation
    */
  override def orientation: Orientation = blocks.head.orientation

  /**
    * Obtain a new [[Feature]] by adding a [[Block]] to this [[Feature]]
    * Returned feature will have block names removed
    *
    * @param block Block to add
    * @param name Name for returned [[Feature]] if different from this
    * @return New [[Feature]] with [[Block]] added
    */
  override def addBlock(block: Block, name: String): Feature = union(block, name)

  /**
    * Get a copy of this [[Feature]] with a new name
    *
    * @param name New name
    * @return New [[Feature]] with same blocks and new name
    */
  override def rename(name: String): Feature = BlockSet(blocks, name)

  /**
    * Check whether two [[Feature]]s are equal except for their names
    *
    * @param feat Other [[Feature]]
    * @return True iff the [[Feature]]s are equal except for their names
    */
  override def equalsIgnoreName(feat: Feature): Boolean = blocks == feat.blocks

  /**
    * Test whether this [[Feature]] contains another [[Feature]]
    *
    * @param feat Other [[Feature]]
    * @return True iff the other [[Feature]] is contained in this [[Feature]]
    */
  override def contains(feat: Feature): Boolean = ???

  /**
    * Get the number of blocks
    *
    * @return The number of blocks
    */
  override def numBlocks: Int = blocks.length

  override def toString: String = {
    val sb = new StringBuilder
    sb.append(name)
    sb.append(':')
    sb.append('[')
    sb.append(blocks.mkString(", "))
    sb.append(']')
    sb.toString()
  }

}