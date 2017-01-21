package feature.format

import feature._

/** A record representing the information contained in one line of a file in GTF2.2 feature.format
  * ([[http://mblab.wustl.edu/GTF22.html feature.format specification]])
  *
  * @param line String containing one valid GTF2.2 line with or without trailing newline character
  */
class GTF22Record(private val line: String) extends FeatureBuilderModifier {

  // Remove trailing comment, then trailing whitespace, then final semicolon
  private val trimmed: String = line.split("#")(0).replaceAll("""\s+$""", "").replaceAll(";$", "")

  // Fields are tab-separated
  private val tokens: Array[String] = trimmed.split("\t")

  validate()

  private def validate(): Unit = {
    if (!isComment) {
      // Can't have two tabs in a row
      if (tokens.contains(""))
        throw new IllegalArgumentException(s"Malformed GTF2.2 line. Empty field.\n$line")
      // Must have at least 9 tab-separated fields
      if (tokens.length < 9)
        throw new IllegalArgumentException(s"Invalid GTF2.2 line. Must have at least 9 tab-separated fields.\n$line")
      if (start >= end)
        throw new IllegalArgumentException(s"Invalid GTF2.2 line. Invalid start and end.\n$line")
      if (start < 0)
        throw new IllegalArgumentException(s"Invalid GTF2.2 line. Invalid start position.\n$line")
      if (frame.isDefined && (frame.get < 0 || frame.get > 2))
        throw new IllegalArgumentException(s"Invalid GTF2.2 line. Invalid frame.\n$line")
      // Compute lazy orientation and score so they get validated
      val o = orientation
      val s = score
      if (!ignore) {
        featureType match {
          case _: MatureRNA =>
            // Check for required attributes gene_id and transcript_id
            if (!attributes.contains("gene_id"))
              throw new IllegalArgumentException(s"Invalid GTF2.2 line. Must have gene_id attribute.\n$line")
            if (!attributes.contains("transcript_id"))
              throw new IllegalArgumentException(s"Invalid GTF2.2 line. Must have transcript_id attribute.\n$line")
          case _ => Unit
        }
      }
    }
  }

  /** True if the line represents a comment (starting with '#'), false otherwise. */
  lazy val isComment: Boolean = line.startsWith("#")

  /** True if the line is to be ignored, either because it is a comment or because
    * the feature type is not supported (see [[http://mblab.wustl.edu/GTF22.html feature.format specification]]).
    */
  lazy val ignore: Boolean = isComment || featureType == Ignore

  /** The chromosome or reference sequence name. */
  lazy val chr: String = tokens(0).replaceFirst("^chr", "")

  /** The annotation source (see [[http://mblab.wustl.edu/GTF22.html feature.format specification]]). */
  lazy val source: String = tokens(1)

  /** The feature type. For valid values see [[http://mblab.wustl.edu/GTF22.html feature.format specification]]. */
  lazy val featureType: FeatureType = FeatureType.forLabel(tokens(2))

  /** Zero-based start position in reference coordinates, inclusive.
    *
    * GTF2.2 feature.format uses 1-based positions. This library uses 0-based positions.
    * Therefore, this value has been converted to a 0-based position. The start position
    * is the first position that is included in the feature.
    */
  lazy val start: Int = Utils.zeroBasedInclusiveStart(tokens(3).toInt)

  /** Zero-based end position in reference coordinates, exclusive.
    *
    * GTF2.2 feature.format uses 1-based positions, and furthermore, uses fully closed
    * intervals so the end position is the last position included in the feature.
    * This library uses 0-based positions and half open intervals, so the end
    * position is the position after the last position included in the feature.
    * Therefore, this value is equal to the integer in the GTF2.2 file, because
    * it is theoretically converted twice for these two differences.
    *
    */
  lazy val end: Int = Utils.zeroBasedExclusiveEnd(tokens(4).toInt)

  /** The floating point score, or None if '.' is specified in the line. */
  lazy val score: Option[Float] = tokens(5) match {
    case "." => None
    case s: String => {
      try {
        Some(s.toFloat)
      } catch {
        case n: NumberFormatException => throw new IllegalArgumentException(s"Invalid GTF2 line. Invalid score.\n$line")
      }
    }
  }

  /** The [[Orientation]].
    *
    * This value is equal to [[Plus]] if '+' is specified in the file or [[Minus]] if
    * '-' is specified.
    *
    */
  lazy val orientation: Orientation = tokens(6) match {
    case "+" => Plus
    case "-" => Minus
    case _ => throw new IllegalArgumentException(s"Invalid GTF2 orientation.\n$line")
  }

  /** The frame, or None if '.' is specified in the file. */
  lazy val frame: Option[Int] = tokens(7) match {
    case "." => None
    case s: String => Some(s.toInt)
  }

  /** Map of attribute name to value.
    *
    * Empty attributes (having value "") are not included in this map.
    *
    * See [[http://mblab.wustl.edu/GTF22.html feature.format specification]] for details on GTF2.2 attributes.
    *
    */
  lazy val attributes: Map[String, String] = {
    // Map of attribute name to attribute value
    val as = new scala.collection.mutable.HashMap[String, String]
    // Attribute key/value pairs are separated by semicolons
    tokens(8).split("; ").foreach(s => {
      // Key and value are separated by a single space
      val ss: Array[String] = s.split(" ")
      if(ss.length < 2) throw new IllegalArgumentException(s"Invalid GTF2 attribute: $s\n$line")
      val k: String = ss.head
      // There can only be one value between double quotes
      if(ss.tail.mkString(" ").split("\"").length > 2)
        throw new IllegalArgumentException(s"Invalid GTF2 attribute spacing:\n$line")
      val v: String = ss.tail.mkString(" ").replace("\"", "") // String values are between double quotes
      if(k == "" || k.startsWith(" ")) throw new IllegalArgumentException(s"Invalid GTF2 attribute spacing: $s\n$line")
      if(v.startsWith(" ")) throw new IllegalArgumentException(s"Invalid GTF2 attribute spacing:\n$line")
      // Skip empty string attribute values
      if(v != "") as.put(k, v)
    })
    // Convert to an immutable map
    as.toMap
  }

  /** Transcript ID, or None if attribute value is "" */
  lazy val transcriptId: Option[String] = attributes.get("transcript_id")

  /** Gene ID, or None if attribute value is "" */
  lazy val geneId: Option[String] = attributes.get("gene_id")

  /** Returns a new [[FeatureBuilder]] consisting of the passed [[FeatureBuilder]] with
    * the information in this [[GTF22Record]] added to it.
    *
    * The feature type must not be one of the CNS types (see [[http://mblab.wustl.edu/GTF22.html feature.format specification]])
    * as these features are specified on a single GTF2.2 line and should not be incorporated into an existing
    * [[FeatureBuilder]]. The feature type must also not be an ignored type.
    *
    * @param fb Original [[FeatureBuilder]] with the same transcript ID and gene ID as this record
    * @return New [[FeatureBuilder]] with additional information from this record incorporated
    */
  override def op(fb: FeatureBuilder): FeatureBuilder = {

    // Verify valid feature type
    featureType match {
      case t: MatureRNA => None
      case _ =>
        throw new IllegalArgumentException("Only feature types representing parts of transcripts can be used" +
        s"to update a ${classOf[FeatureBuilder].getName}. GTF2.2 line:\n$line")
    }

    // Only operate on the FeatureBuilder if it has the same transcript ID and gene ID as this record
    if(transcriptId != fb.featureId)
      throw new IllegalArgumentException(s"Transcript ID ($transcriptId) must be equal to existing feature ID (${fb.featureId})")

    if(geneId != fb.geneId)
      throw new IllegalArgumentException(s"Gene ID ($geneId) must be equal to existing gene ID (${fb.geneId})")

    // Create the block for this record
    val blk = Block(chr, start, end, orientation)

    featureType match {
      // Assume the CDS will be specified so don't do anything special with start codon, 5'-UTR, 3'-UTR
      case Exon | StartCodon | UTR5 | UTR3 => fb.addBlock(blk)
      // Include the stop codon in our CDS
      case CDS | StopCodon =>
        fb.addBlock(blk).setCdsStart(Utils.updateCdsStart(fb, start)).setCdsEnd(Utils.updateCdsEnd(fb, end))
      // These feature types must have only one block
      case Intergenic | IntergenicCNS | IntronicCNS =>
        if(fb.blocks.nonEmpty) throw new IllegalArgumentException(
          s"Feature builder should have no blocks when encountering record\n$line")
        else fb.addBlock(blk)
      case Ignore => throw new IllegalArgumentException(s"Invalid feature type: $featureType")
    }

  }

}


private object Utils {

  /*
   Update CDS start after encountering a new block.
   Replace existing start if new start is less, or add new start if doesn't exist yet
    */
  def updateCdsStart(fb: FeatureBuilder, ns: Int): Int = if(fb.cdsStart.isDefined) math.min(ns, fb.cdsStart.get) else ns

  /*
   Update CDS end after encountering a new block.
   Replace existing end if new end is greater, or add new end if doesn't exist yet
    */
  def updateCdsEnd(fb: FeatureBuilder, ne: Int): Int = if(fb.cdsEnd.isDefined) math.max(ne, fb.cdsEnd.get) else ne

  def zeroBasedInclusiveStart(gtf2start: Int): Int = gtf2start - 1

  def zeroBasedExclusiveEnd(gtf2end: Int): Int = gtf2end

}

/** A GTF2.2 feature type */
sealed trait FeatureType {val name: String}

/** A GTF2.2 feature type that represents part of a mature RNA transcript */
sealed trait MatureRNA extends FeatureType

/** GTF2.2 CDS feature type */
case object CDS extends MatureRNA {val name = "CDS"}

/** GTF2.2 start codon feature type */
case object StartCodon extends MatureRNA {val name = "start_codon"}

/** GTF2.2 stop codon feature type */
case object StopCodon extends MatureRNA {val name = "stop_codon"}

/** GTF2.2 5'-UTR feature type */
case object UTR5 extends MatureRNA {val name = "5UTR"}

/** GTF2.2 3'-UTR feature type */
case object UTR3 extends MatureRNA {val name = "3UTR"}

/** GTF2.2 intergenic feature type */
case object Intergenic extends FeatureType {val name = "inter"}

/** GTF2.2 intergenic CNS feature type */
case object IntergenicCNS extends FeatureType {val name = "inter_CNS"}

/** GTF2.2 intronic CNS feature type */
case object IntronicCNS extends FeatureType {val name = "intron_CNS"}

/** GTF2.2 exon feature type */
case object Exon extends MatureRNA {val name = "exon"}

/** Nonstandard feature type. See [[http://mblab.wustl.edu/GTF22.html feature.format specification]] for details. */
case object Ignore extends FeatureType {val name = "ignore"}

/** Utilities for GTF2.2 [[FeatureType]]s */
object FeatureType {

  /** Returns the [[FeatureType]] specified by the GTF2.2 value
    *
    * @param label GTF2.2 feature type label
    */
  def forLabel(label: String): FeatureType = {
    label match {
      case Exon.name => Exon
      case CDS.name => CDS
      case StartCodon.name => StartCodon
      case StopCodon.name => StopCodon
      case UTR5.name => UTR5
      case UTR3.name => UTR3
      case Intergenic.name => Intergenic
      case IntergenicCNS.name => IntergenicCNS
      case IntronicCNS.name => IntronicCNS
      case _ => Ignore
    }
  }
}