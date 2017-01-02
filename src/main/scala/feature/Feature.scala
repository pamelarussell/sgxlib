package feature

import feature.Exceptions.CDSSizeException

/** A genomic feature.
  *
  * Includes a non-empty underlying [[Region]] and an optional feature name.
  *
  * @param blocks Non-empty underlying [[Region]]
  * @param name Optional feature name
  */
sealed abstract class Feature(val blocks: Region, val name: Option[String]) extends Ordered[Feature] {

  // Require that region be nonempty
  if(blocks.isEmpty) throw new IllegalArgumentException("Region must be nonempty")

  /** Returns the chromosome name. */
  def getChr: String = blocks.chr

  /** Returns the start position of the underlying [[Region]] as in [[Region.start]]. */
  def getStart: Int = blocks.start

  /** Returns the end position of the underlying [[Region]] as in [[Region.end]]. */
  def getEnd: Int = blocks.end

  /** Returns the orientation of the underlying [[Region]] as in [[Region.orientation]]. */
  def getOrientation: Orientation = blocks.orientation

  /** Returns the list of [[Block]]s of the underlying [[Region]] as in [[Region.blocks]]. */
  def getBlocks: List[Block] = blocks.blocks

  /** Returns a boolean value representing whether this [[Feature]] overlaps another [[Feature]].
    *
    * Ignores feature name and calls [[Region.overlaps]] on the two underlying [[Region]]s.
    *
    * @param other Other [[Feature]]
    * @return True if the [[Feature]]s overlap, false otherwise
    */
  def overlaps(other: Feature): Boolean = blocks.overlaps(other.blocks)

  /** Returns a boolean value representing whether this [[Feature]] contains another [[Feature]].
    *
    * Ignores feature name and calls [[Region.contains]] on the two underlying [[Region]]s.
    *
    * @param other Other [[Feature]]
    * @return True if this [[Feature]] contains the other, false otherwise
    */
  def contains(other: Feature): Boolean = blocks.contains(other.blocks)

  /** Returns a [[Feature]] representing the union of this with another [[Feature]].
    *
    * See subclass documentation for class-specific details.
    *
    * @param other Other [[Feature]]
    * @return [[Feature]] representing the union of the two [[Feature]]s
    */
  def union(other: Feature): Feature

  /** Returns a [[Feature]] representing the intersection of this with another [[Feature]].
    *
    * See subclass documentation for class-specific details.
    *
    * @param other Other [[Feature]]
    * @return [[Feature]] representing the intersection of the two [[Feature]]s, or [[None]] if they do not overlap
    */
  def intersection(other: Feature): Option[Feature]

  /** Returns a [[Feature]] representing this minus the overlap with another [[Feature]].
    *
    * See subclass documentation for class-specific details.
    *
    * @param other Other [[Feature]]
    * @return [[Feature]] representing this minus the other, or [[None]] if the remaining region is empty
    */
  def minus(other: Feature): Option[Feature]

  /** Returns a [[Feature]] representing this with an additional [[Block]] merged in.
    *
    * See subclass documentation for class-specific details.
    *
    * @param block [[Block]] to add
    * @return New [[Feature]] with the new [[Block]] merged in
    */
  def addBlock(block: Block): Feature

  /** Returns the number of [[Block]]s in the underlying region. */
  def numBlocks: Int = blocks.numBlocks

}

// Some private functions
private object Feature {

  def compare(f1: Feature, f2: Feature): Int = {
    val rc = f1.blocks compare f2.blocks
    if(rc != 0) rc // If regions are different compare regions
    else optStrCompare(f1.name, f2.name) // Else compare names
  }

  // A comparison function for Option[String]
  def optStrCompare(s1: Option[String], s2: Option[String]): Int = {
    // Name defined comes before name not defined
    if(s1.isDefined) {
      if(s2.isDefined) scala.math.Ordering.String.compare(s1.toString, s2.toString) // s1 defined, s2 defined
      else -1 // s1 defined, s2 not defined
    } else {
      if(s2.isDefined) 1 // s1 not defined, s2 defined
      else 0 // s1 not defined, s2 not defined
    }
  }

}

/** A [[Feature]] consisting of an underlying [[Region]] and an optional name.
  *
  * @param blocks Non-empty underlying [[Region]]
  * @param name Optional feature name. Do not pass Some(""); use None in that case.
  */
class GenericFeature(override val blocks: Region, override val name: Option[String]) extends Feature(blocks, name) {

  if(name == Some("")) throw new IllegalArgumentException("Name cannot be empty string. Use None instead.")

  /** Returns a [[GenericFeature]] representing the union of this with another [[Feature]].
    *
    * The returned feature has underlying [[Region]] equal to the union of the two underlying [[Region]]s
    * as computed by [[Region.union]]. The returned feature has name None.
    *
    * @param other Other [[Feature]]
    * @return [[GenericFeature]] calculated as the union of the underlying [[Region]]s, with name None
    */
  final override def union(other: Feature): Feature = new GenericFeature(blocks.union(other.blocks), None)

  /** Returns a [[GenericFeature]] representing the intersection of this with another [[Feature]].
    *
    * If the underlying [[Region]]s do not overlap according to [[Region.overlaps]], returns None.
    *
    * Otherwise, the returned feature has underlying [[Region]] equal to the intersection of the two underlying [[Region]]s
    * as computed by [[Region.intersection]]. The returned feature has name None.
    *
    * @param other Other [[Feature]]
    * @return [[GenericFeature]] calculated as the intersection of the underlying [[Region]]s with name None,
    *        or None if the underlying [[Region]]s do not overlap
    */
  final override def intersection(other: Feature): Option[Feature] = {
    val in = blocks.intersection(other.blocks)
    if(in.isEmpty) None else Some(new GenericFeature(in, None))

  }

  /** Returns a [[GenericFeature]] representing this minus the overlap with another [[Feature]].
    *
    * If the underlying [[Region]] of this [[Feature]] is contained in the underlying [[Region]] of the other,
    * returns None.
    *
    * Otherwise, the returned [[Feature]] has underlying [[Region]] equal to the result of [[Region.minus]] on the
    * two underlying [[Region]]s. The returned [[Feature]] has name None.
    *
    * @param other Other [[Feature]]
    * @return [[GenericFeature]] calculated as the subtraction of the underlying [[Region]]s with name None,
    *        or None if that subtraction is empty
    */
  final override def minus(other: Feature): Option[Feature] = {
    val mn = blocks.minus(other.blocks)
    if(mn.isEmpty) None else Some(new GenericFeature(mn, None))
  }

  /** Returns a [[GenericFeature]] representing this with a new [[Block]] merged in.
    *
    * The underlying [[Region]] of the returned [[Feature]] is the result of calling [[Region.addBlock]] on the
    * underlying [[Region]] of this [[Feature]]. The name of the returned [[Feature]] is None.
    *
    * @param block [[Block]] to add
    * @return [[GenericFeature]] whose underlying [[Region]] has the new [[Block]] merged in, with name None
    */
  final override def addBlock(block: Block): Feature = new GenericFeature(blocks.addBlock(block), None)

  /** Returns true if other is an instance of [[GenericFeature]], false otherwise. */
  def canEqual(other: Any): Boolean = other.isInstanceOf[GenericFeature]

  /** Returns an equality comparison of this with another object.
    *
    * Returns true if other is a [[GenericFeature]] that can equal this according to [[GenericFeature.canEqual]],
    * the underlying [[Region]]s are equal, and the names are equal. Returns false otherwise.
    *
    * @param other Other object
    * @return True if this equals other, false otherwise
    */
  override def equals(other: Any): Boolean = {
    other match {
      case that: GenericFeature => that.canEqual(this) && that.blocks == this.blocks && that.name == this.name
      case _ => false
    }
  }

  // Calculate a hashCode
  protected def hc: Int = (blocks, name).##

  /** A hashCode based on the underlying [[Region]] and [[name]]. */
  override lazy val hashCode: Int = hc

  /** Returns the result of comparing this [[GenericFeature]] to another [[Feature]].
    *
    * First compare the underlying [[Region]]s with [[Region.compare]]. If that result is non-zero, return it.
    *
    * Next, compare the names.
    * If both names are defined, use regular ordering on Strings. Otherwise, compare according to an ordering where name defined comes before name not defined.
    * If the name comparison is non-zero, return it.
    *
    * Next, compare the classes. The ordering used is [[GenericFeature]] < [[Transcript]] < [[MessengerRNA]].
    *
    * @param that Other [[Feature]] to compare
    * @return Negative integer if this is less than other, zero if neither is greater, positive integer if this is greater than other
    */
  override def compare(that: Feature): Int = {
    val c = Feature.compare(this, that)
    if(c != 0) c
    else {
      that match {
        case mr: MessengerRNA => -1
        case tr: Transcript => -1
        case gf: GenericFeature => c
      }
    }
  }

  /** Returns a string representation of this [[GenericFeature]]. */
  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb append "("
    sb append this.getClass.getName
    sb append blocks.toString
    sb append ","
    sb append name.toString
    sb append ")"
    sb.toString()
  }


}

/** A representation of a spliced transcript.
  *
  * A [[Transcript]] has an underlying [[Region]] that specifies the chromosome, [[Block]]s, and [[Orientation]].
  * In addition, a [[Transcript]] has an optional name and parent gene name.
  *
  * @param blocks Non-empty underlying [[Region]]
  * @param name Optional feature name. Do not pass Some(""); use None in that case.
  * @param geneId Optional parent gene name. Do not pass Some(""); use None in that case.
  */
sealed class Transcript(override val blocks: Region, name: Option[String], val geneId: Option[String]) extends GenericFeature(blocks, name) {

  validateParams()

  // Function to validate the parameters
  private def validateParams(): Unit = {
    if(name.contains("")) throw new IllegalArgumentException("Name cannot be empty string. Use None instead.")
    if(geneId.contains("")) throw new IllegalArgumentException("geneId cannot be empty string. Use None instead.")
    val or = getOrientation
    // Require that orientation be positive or negative
    if(or != Plus && or != Minus) throw new IllegalArgumentException(s"Invalid orientation: $getOrientation. Options: ${Plus.toString}, ${Minus.toString}")
  }

  /** Returns a list of [[Block]]s representing the introns of this [[Transcript]].
    *
    * The returned list is in order from left to right. Each [[Block]] in the returned list is the span of a gap
    * between two [[Block]]s of this [[Transcript]]. The returned [[Block]]s have the same [[Orientation]]
    * as this [[Transcript]]. The start position of each returned [[Block]] is equal to the end position (exclusive)
    * of the previous adjacent [[Block]] in this [[Transcript]]. The end position of each returned [[Block]] (exclusive)
    * is equal to the start position of the following adjacent [[Block]] in this [[Transcript]].
    *
    * If this [[Transcript]] has only one [[Block]], Nil is returned.
    *
    * @return A list of [[Block]]s representing the gaps between the [[Block]]s of this [[Transcript]], or Nil if this
    *         [[Transcript]] has only one [[Block]].
    */
  def getIntrons: List[Block] = {
    if(numBlocks < 2) Nil
    else {
      val blockStarts: List[Int] = blocks.blocks.map(b => b.start)
      val blockEnds: List[Int] = blocks.blocks.map(b => b.end)
      blockEnds.zip(blockStarts.tail).map(p => Block(getChr, p._1, p._2, getOrientation))
    }
  }

  /** Returns true if other is an instance of [[Transcript]], false otherwise. */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Transcript]

  /** Returns an equality comparison of this with another object.
    *
    * Returns true if other is a [[Transcript]] that can equal this according to [[Transcript.canEqual]],
    * the underlying [[Region]]s are equal, the names are equal, and the gene IDs are equal. Returns false otherwise.
    *
    * @param other Other object
    * @return True if this equals other, false otherwise
    */
  override def equals(other: Any): Boolean = {
    other match {
      case that: Transcript => super.equals(that) && geneId == that.geneId
      case _ => false
    }
  }

  /** A hashCode based on the underlying [[Region]], [[name]], and [[geneId]]. */
  override lazy val hashCode: Int = (super.hc, geneId).##

  /** Returns a string representation of this [[Transcript]]. */
  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb append "("
    sb append this.getClass.getName
    sb append blocks.toString
    sb append ","
    sb append name.toString
    sb append ","
    sb append geneId.toString
    sb append ")"
    sb.toString()
  }

  /** Returns the result of comparing this [[Transcript]] to another [[Feature]].
    *
    * First compare the underlying [[Region]]s with [[Region.compare]]. If that result is non-zero, return it.
    *
    * Next, compare the names.
    * If both names are defined, use regular ordering on Strings. Otherwise, compare according to an ordering where name defined comes before name not defined.
    * If the name comparison is non-zero, return it.
    *
    * Next, if the other is a [[GenericFeature]], return a positive integer.
    *
    * Otherwise, compare the gene IDs.
    * If both gene IDs are defined, use regular ordering on Strings. Otherwise, compare according to an ordering where gene ID defined comes before gene ID not defined.
    * If the gene IDs comparison is non-zero, return it.
    *
    * If the gene ID comparison was zero, compare the classes. The ordering used is [[GenericFeature]] < [[Transcript]] < [[MessengerRNA]].
    *
    * @param that Other [[Feature]] to compare
    * @return Negative integer if this is less than other, zero if neither is greater, positive integer if this is greater than other
    */
  override def compare(that: Feature): Int = {
    val c = Feature.compare(this, that)
    if(c != 0) c
    else {
      that match {
        case mr: MessengerRNA =>
          val gc = Feature.optStrCompare(geneId, mr.geneId)
          if(gc != 0) gc
          else -1
        case tr: Transcript => Feature.optStrCompare(geneId, tr.geneId)
        case gf: GenericFeature => 1
      }
    }
  }

}

/** A representation of a spliced messenger RNA.
  *
  * A [[MessengerRNA]] has an underlying [[Region]] that specifies the chromosome, [[Block]]s, and [[Orientation]].
  * In addition, a [[MessengerRNA]] has an optional name and parent gene name.
  * Finally, it also has CDS start and end positions.
  *
  * @param blocks Non-empty underlying [[Region]]
  * @param cdsStart Zero-based CDS start position (inclusive).
  *                 The CDS start is the smallest (leftmost) position of the CDS, regardless of transcript [[Orientation]].
  *                 In other words, if [[Orientation]] is [[Plus]], the CDS start is the first position of the start codon.
  *                 If [[Orientation]] is [[Minus]], the CDS start is the 3'-most position of the stop codon.
  *                 The CDS start must lie within one of the [[Block]]s.
  *                 The CDS must be at least 6 nucleotides in length (after splicing) and its length must be a multiple of 3.
  *                 Otherwise, an IllegalArgumentException is thrown.
  * @param cdsEnd Zero-based CDS end position (exclusive).
  *               The CDS end is one plus the largest (rightmost) position of the CDS, regardless of transcript [[Orientation]].
  *               In other words, if [[Orientation]] is [[Plus]], the CDS end is one plus the last position of the stop codon.
  *               If [[Orientation]] is [[Minus]], the CDS end is the 5'-most position of the start codon.
  *               The CDS end must lie within one of the [[Block]]s or be equal to one plus a [[Block]] end position.
  *               The CDS must be at least 6 nucleotides in length (after splicing) and its length must be a multiple of 3.
  *               Otherwise, an IllegalArgumentException is thrown.
  * @param name Optional feature name. Do not pass Some(""); use None in that case.
  * @param geneId Optional parent gene name. Do not pass Some(""); use None in that case.
  */
final case class MessengerRNA(override val blocks: Region, cdsStart: Int, cdsEnd: Int, override val name: Option[String],
                              override val geneId: Option[String])
  extends Transcript(blocks, name, geneId) {

  /** A [[Region]] representing the coding region of this [[MessengerRNA]].
    *
    * The CDS includes the start and stop codons. It is equal to the value of calling [[Region.trim]]([[cdsStart]], [[cdsEnd]])
    * on the underlying [[Region]] of this [[MessengerRNA]].
    */
  lazy val getCDS: Region = blocks.trim(cdsStart, cdsEnd)

  validateParams()

  // Function to validate the constructor parameters
  private def validateParams(): Unit = {

    if(name.contains("")) throw new IllegalArgumentException("Name cannot be empty string. Use None instead.")
    if(geneId.contains("")) throw new IllegalArgumentException("geneId cannot be empty string. Use None instead.")

    // Require that CDS start be at least feature start
    if(cdsStart < getStart) throw new IllegalArgumentException(
      s"CDS start ($cdsStart) must be >= feature start ($getStart) (name: ${name.toString})")
    // Require that CDS end be at most feature end
    if(cdsEnd > getEnd) throw new IllegalArgumentException(
      s"CDS end ($cdsEnd) must be <= feature end ($getEnd) (name: ${name.toString})")
    // Require that CDS start be less than CDS end
    if(cdsStart >= cdsEnd) throw new IllegalArgumentException(
      s"CDS start must be < CDS end ($cdsStart, $cdsEnd) (name: ${name.toString})")
    val chr = getChr
    val or = getOrientation

    // Validate CDS start
    val st = Block(chr, cdsStart, cdsStart + 1, or) // Start position
    // Require that CDS start overlap feature
    if(!st.overlaps(blocks)) throw new IllegalArgumentException(
      s"CDS start ($cdsStart) must overlap blocks (${blocks.toString}) (name: ${name.toString})")

    // Validate CDS end
    val en = Block(chr, cdsEnd, cdsEnd + 1, or) // End position
    // CDS end cannot be a block start
    if(blocks.blocks.exists(blk => cdsEnd == blk.start)) throw new IllegalArgumentException(
      s"CDS end ($cdsStart) can't be a block start (${blocks.toString}) (name: ${name.toString})")
    // CDS end must either overlap a block or be a block end
    if(!en.overlaps(blocks)) {
      if(!blocks.blocks.exists(blk => cdsEnd == blk.end)) {
        throw new IllegalArgumentException(s"CDS end ($cdsStart) must overlap blocks " +
          s"(${blocks.toString}) or equal end of a block (name: ${name.toString})")
      }
    }

    // Validate CDS length
    val cdsSize = getCDS.size
    // CDS size must be at least 6
    if(cdsSize < 6) throw new CDSSizeException(s"CDS size must be at least 6 " +
      s"(CDS size: $cdsSize, name: ${name.toString})")
    // CDS size must be divisible by 3
    if(cdsSize % 3 != 0) throw new CDSSizeException(
      s"CDS size must be divisible by 3 (CDS size: $cdsSize, name: ${name.toString})")

  }

  /** Returns a [[Region]] representing the 3'-UTR of this [[MessengerRNA]].
    *
    * If the [[Orientation]] is [[Plus]], the 3'-UTR has start position [[cdsEnd]] and end position equal
    * to [[getEnd]]. In other words, the 3'-UTR is equal to the value of calling [[Region.trim]]([[cdsEnd]], [[getEnd]])
    * on the underlying [[Region]] of this [[MessengerRNA]].
    *
    * If the [[Orientation]] is [[Minus]], the 3'-UTR has start position [[getStart]] and end position equal to
    * [[cdsStart]]. In other words, the 3'-UTR is equal to the value of calling [[Region.trim]]([[getStart]], [[cdsStart]])
    * on the underlying [[Region]] of this [[MessengerRNA]].
    *
    * If the stop codon is at one end of the [[MessengerRNA]], returns None.
    *
    * @return A [[Region]] representing the 3'-UTR, or None if the stop codon is at one end of the [[MessengerRNA]].
    */
  def get3UTR: Option[Region] = {
    getOrientation match {
      case Plus =>
        if(cdsEnd == getEnd) None
        else Some(blocks.trim(cdsEnd, getEnd))
      case Minus =>
        if(cdsStart == getStart) None
        else Some(blocks.trim(getStart, cdsStart))
      case _ => throw new IllegalStateException(s"Invalid orientation: $getOrientation")
    }
  }

  /** Returns a [[Region]] representing the 5'-UTR of this [[MessengerRNA]].
    *
    * If the [[Orientation]] is [[Plus]], the 5'-UTR has start position [[getStart]] and end position [[cdsStart]].
    * In other words, the 5'-UTR is equal to the value of calling [[Region.trim]]([[getStart]], [[cdsStart]])
    * on the underlying [[Region]] of this [[MessengerRNA]].
    *
    * If the [[Orientation]] is [[Minus]], the 5'-UTR has start position [[cdsEnd]] and end position
    * [[getEnd]]. In other words, the 5'-UTR is equal to the value of calling [[Region.trim]]([[cdsEnd]], [[getEnd]])
    * on the underlying [[Region]] of this [[MessengerRNA]].
    *
    * If the start codon is at one end of the [[MessengerRNA]], returns None.
    *
    * @return A [[Region]] representing the 5'-UTR, or None if the start codon is at one end of the [[MessengerRNA]].
    */
  def get5UTR: Option[Region] = {
    getOrientation match {
      case Plus =>
        if(cdsStart == getStart) None
        else Some(blocks.trim(getStart, cdsStart))
      case Minus =>
        if(cdsEnd == getEnd) None
        else Some(blocks.trim(cdsEnd, getEnd))
      case _ => throw new IllegalStateException(s"Invalid orientation: $getOrientation")
    }
  }

  private def shiftChrPos(origPos: Int, relShift: Int): Int = {
    val relPos = blocks.relativePos(origPos)
    if(relPos.isEmpty) throw new IllegalArgumentException("Invalid relative position")
    val origRel = relPos.get
    blocks.chrPos(origRel + relShift)
  }

  /** Returns a [[Region]] representing the start codon of this [[MessengerRNA]].
    *
    * If the [[Orientation]] is [[Plus]], the start codon has start position [[cdsStart]]
    * and size 3. If the orientation is [[Minus]], the start codon has end position [[cdsEnd]]
    * and size 3.
    *
    * The start codon may include multiple [[Block]]s if the CDS boundary lies near a [[Block]] boundary.
    *
    * @return A [[Region]] of size 3, possibly spliced, representing the start codon of this [[MessengerRNA]].
    */
  def getStartCodon: Region = {
    getOrientation match {
      case Plus => blocks.trim(cdsStart, shiftChrPos(cdsStart, 3))
      case Minus => blocks.trim(shiftChrPos(cdsEnd - 1, 2), cdsEnd)
      case _ => throw new IllegalStateException(s"Invalid orientation: $getOrientation")
    }
  }

  /** Returns a [[Region]] representing the stop codon of this [[MessengerRNA]].
    *
    * If the [[Orientation]] is [[Plus]], the stop codon has end position [[cdsEnd]]
    * and size 3. If the [[Orientation]] is [[Minus]], the stop codon has start position [[cdsStart]]
    * and size 3.
    *
    * The stop codon may include multiple [[Block]]s if the CDS boundary lies near a [[Block]] boundary.
    *
    * @return A [[Region]] of size 3, possibly spliced, representing the stop codon of this [[MessengerRNA]].
    */
  def getStopCodon: Region = {
    getOrientation match {
      case Plus => blocks.trim(shiftChrPos(cdsEnd - 1, -2), cdsEnd)
      case Minus => blocks.trim(cdsStart, shiftChrPos(cdsStart, -3))
      case _ => throw new IllegalStateException(s"Invalid orientation: $getOrientation")
    }
  }

  /** Returns true if other is an instance of [[MessengerRNA]], false otherwise. */
  override def canEqual(other: Any): Boolean = other.isInstanceOf[MessengerRNA]

  /** A hashCode based on the underlying [[Region]], [[name]], [[geneId]], [[cdsStart]], and [[cdsEnd]]. */
  override lazy val hashCode: Int = (super.hc, cdsStart, cdsEnd).##

  /** Returns an equality comparison of this with another object.
    *
    * Returns true if other is a [[MessengerRNA]] that can equal this according to [[MessengerRNA.canEqual]],
    * the underlying [[Region]]s are equal, the names are equal, the gene IDs are equal, the CDS starts are equal,
    * and the CDS ends are equal. Returns false otherwise.
    *
    * @param other Other object
    * @return True if this equals other, false otherwise
    */
  override def equals(other: Any): Boolean = {
    other match {
      case that: MessengerRNA => super.equals(that) && cdsStart == that.cdsStart && cdsEnd == that.cdsEnd
      case _ => false
    }
  }

  /** Returns a string representation of this [[MessengerRNA]]. */
  override def toString: String = {
    val sb: StringBuilder = new StringBuilder
    sb append "("
    sb append this.getClass.getName
    sb append blocks.toString
    sb append ","
    sb append name.toString
    sb append ","
    sb append geneId.toString
    sb append ",CDS:"
    sb append cdsStart
    sb append "-"
    sb append cdsEnd
    sb append ")"
    sb.toString()
  }

  /** Returns the result of comparing this [[MessengerRNA]] to another [[Feature]].
    *
    * First compare the underlying [[Region]]s with [[Region.compare]]. If that result is non-zero, return it.
    *
    * Next, compare the names.
    * If both names are defined, use regular ordering on Strings. Otherwise, compare according to an ordering where name defined comes before name not defined.
    * If the name comparison is non-zero, return it.
    *
    * Next, if the other is a [[GenericFeature]], return a positive integer.
    *
    * Otherwise, compare the gene IDs.
    * If both gene IDs are defined, use regular ordering on Strings. Otherwise, compare according to an ordering where gene ID defined comes before gene ID not defined.
    * If the gene IDs comparison is non-zero, return it.
    *
    * If the gene ID comparison was zero and other is a [[Transcript]], return a positive integer.
    *
    * If the gene ID comparison was zero and other is a [[MessengerRNA]], compare the CDS start position, then finally the CDS end position.
    *
    * @param that Other [[Feature]] to compare
    * @return Negative integer if this is less than other, zero if neither is greater, positive integer if this is greater than other
    */
  override def compare(that: Feature): Int = {
    val c = Feature.compare(this, that)
    if(c != 0) c
    else that match {
      case mr: MessengerRNA =>
        val gc = Feature.optStrCompare(geneId, mr.geneId)
        if(gc != 0) gc
        else {
          val sc = cdsStart - mr.cdsStart
          if(sc != 0) sc
          else cdsEnd - mr.cdsEnd
        }
      case tr: Transcript =>
        val gc = Feature.optStrCompare(geneId, tr.geneId)
        if(gc != 0) gc
        else 1
      case gf: GenericFeature => 1
    }
  }

}

/** A gene giving rise to one or more [[Transcript]]s.
  *
  * @param transcripts Non-empty List of [[Transcript]]s
  * @param name Gene name
  */
final case class Gene(transcripts: List[Transcript], name: String)
