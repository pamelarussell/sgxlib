package feature

sealed abstract class Feature(val blocks: Region, val name: Option[String]) extends Ordered[Feature] {

  // Require that region be nonempty
  if(blocks.isEmpty) throw new IllegalArgumentException("Region must be nonempty")

  def getChr: String = blocks.chr

  def getStart: Int = blocks.start

  def getEnd: Int = blocks.end

  def getOrientation: Orientation = blocks.orientation

  def getBlocks: List[Block] = blocks.blocks

  def overlaps(other: Feature): Boolean = blocks.overlaps(other.blocks)

  def contains(other: Feature): Boolean = blocks.contains(other.blocks)

  def union(other: Feature): Feature

  def intersection(other: Feature): Option[Feature]

  def minus(other: Feature): Option[Feature]

  def addBlock(block: Block): Feature

  def numBlocks: Int = blocks.numBlocks

  override def compare(that: Feature): Int

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

sealed class GenericFeature(override val blocks: Region, override val name: Option[String]) extends Feature(blocks, name) {

  if(name == Some("")) throw new IllegalArgumentException("Name cannot be empty string. Use None instead.")

  final override def union(other: Feature): Feature = new GenericFeature(blocks.union(other.blocks), None)

  final override def intersection(other: Feature): Option[Feature] = {
    val in = blocks.intersection(other.blocks)
    if(in.isEmpty) None else Some(new GenericFeature(in, None))

  }

  final override def minus(other: Feature): Option[Feature] = {
    val mn = blocks.minus(other.blocks)
    if(mn.isEmpty) None else Some(new GenericFeature(mn, None))
  }

  final override def addBlock(block: Block): Feature = new GenericFeature(blocks.addBlock(block), None)

  def canEqual(other: Any): Boolean = other.isInstanceOf[GenericFeature]

  override def equals(other: Any): Boolean = {
    other match {
      case that: GenericFeature => that.canEqual(this) && that.blocks == this.blocks && that.name == this.name
      case _ => false
    }
  }

  // Calculate a hashCode
  protected def hc: Int = (blocks, name).##

  // Cache the hashCode
  override lazy val hashCode: Int = hc

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

sealed class Transcript(override val blocks: Region, name: Option[String], val geneId: Option[String]) extends GenericFeature(blocks, name) {

  validateParams()

  // Function to validate the parameters
  private def validateParams(): Unit = {
    if(name == Some("")) throw new IllegalArgumentException("Name cannot be empty string. Use None instead.")
    if(geneId == Some("")) throw new IllegalArgumentException("geneId cannot be empty string. Use None instead.")
    val or = getOrientation
    // Require that orientation be positive or negative
    if(or != Plus && or != Minus) throw new IllegalArgumentException(s"Invalid orientation: $getOrientation. Options: ${Plus.toString}, ${Minus.toString}")
  }

  def getIntrons: List[Block] = {
    if(numBlocks < 2) Nil
    else {
      val blockStarts: List[Int] = blocks.blocks.map(b => b.start)
      val blockEnds: List[Int] = blocks.blocks.map(b => b.end)
      blockEnds.zip(blockStarts.tail).map(p => Block(getChr, p._1, p._2, getOrientation))
    }
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Transcript]

  override def equals(other: Any): Boolean = {
    other match {
      case that: Transcript => super.equals(that) && geneId == that.geneId
      case _ => false
    }
  }

  override lazy val hashCode: Int = (super.hc, geneId).##

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

final case class MessengerRNA(override val blocks: Region, cdsStart: Int, cdsEnd: Int, override val name: Option[String],
                              override val geneId: Option[String])
  extends Transcript(blocks, name, geneId) {

  lazy val getCDS: Region = blocks.trim(cdsStart, cdsEnd)

  validateParams()

  // Function to validate the constructor parameters
  private def validateParams(): Unit = {

    if(name == Some("")) throw new IllegalArgumentException("Name cannot be empty string. Use None instead.")
    if(geneId == Some("")) throw new IllegalArgumentException("geneId cannot be empty string. Use None instead.")

    // Require that CDS start be at least feature start
    if(cdsStart < getStart) throw new IllegalArgumentException(s"CDS start ($cdsStart) must be >= feature start ($getStart)")
    // Require that CDS end be at most feature end
    if(cdsEnd > getEnd) throw new IllegalArgumentException(s"CDS end ($cdsEnd) must be <= feature end ($getEnd)")
    // Require that CDS start be less than CDS end
    if(cdsStart >= cdsEnd) throw new IllegalArgumentException(s"CDS start must be < CDS end ($cdsStart, $cdsEnd)")
    val chr = getChr
    val or = getOrientation

    // Validate CDS start
    val st = Block(chr, cdsStart, cdsStart + 1, or) // Start position
    // Require that CDS start overlap feature
    if(!st.overlaps(blocks)) throw new IllegalArgumentException(s"CDS start ($cdsStart) must overlap blocks (${blocks.toString})")

    // Validate CDS end
    val en = Block(chr, cdsEnd, cdsEnd + 1, or) // End position
    // CDS end cannot be a block start
    if(blocks.blocks.exists(blk => cdsEnd == blk.start)) throw new IllegalArgumentException(s"CDS end ($cdsStart) can't be a block start (${blocks.toString})")
    // CDS end must either overlap a block or be a block end
    if(!en.overlaps(blocks)) {
      if(!blocks.blocks.exists(blk => cdsEnd == blk.end)) {
        throw new IllegalArgumentException(s"CDS end ($cdsStart) must overlap blocks (${blocks.toString}) or equal end of a block")
      }
    }

    // Validate CDS length
    val cdsSize = getCDS.size
    // CDS size must be at least 6
    if(cdsSize < 6) throw new IllegalArgumentException("CDS size must be at least 6")
    // CDS size must be divisible by 3
    if(cdsSize % 3 != 0) throw new IllegalArgumentException("CDS size must be divisible by 3")

  }

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

  def getStartCodon: Region = {
    getOrientation match {
      case Plus => blocks.trim(cdsStart, shiftChrPos(cdsStart, 3))
      case Minus => blocks.trim(shiftChrPos(cdsEnd - 1, 2), cdsEnd)
      case _ => throw new IllegalStateException(s"Invalid orientation: $getOrientation")
    }
  }

  def getStopCodon: Region = {
    getOrientation match {
      case Plus => blocks.trim(shiftChrPos(cdsEnd - 1, -2), cdsEnd)
      case Minus => blocks.trim(cdsStart, shiftChrPos(cdsStart, -3))
      case _ => throw new IllegalStateException(s"Invalid orientation: $getOrientation")
    }
  }

  override def canEqual(other: Any): Boolean = other.isInstanceOf[MessengerRNA]

  override lazy val hashCode: Int = (super.hc, cdsStart, cdsEnd).##

  override def equals(other: Any): Boolean = {
    other match {
      case that: MessengerRNA => super.equals(that) && cdsStart == that.cdsStart && cdsEnd == that.cdsEnd
      case _ => false
    }
  }


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

final case class Gene(transcripts: List[Transcript], name: String)
