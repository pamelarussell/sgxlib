package reader

import feature._

class GTF2Record(private val line: String) extends FeatureBuilderModifier {

  private val tokens: Array[String] = line.split("\t")
  validate()

  private def validate(): Unit = {
    if(!ignore) {
      if(tokens.length < 9)
        throw new IllegalArgumentException(s"Invalid GTF2 line. Must have at least 9 tab-separated fields.\n$line")
      val attr = tokens(8).split("; ")
      if(attr.length < 2)
        throw new IllegalArgumentException(s"Invalid GTF2 line. Must have at least 2 semicolon-separated attributes.\n$line")
      if(!attr.exists(_.startsWith("gene_id")))
        throw new IllegalArgumentException(s"Invalid GTF2 line. Must have gene_id attribute.\n$line")
      if(!attr.exists(_.startsWith("transcript_id")))
        throw new IllegalArgumentException(s"Invalid GTF2 line. Must have transcript_id attribute.\n$line")
    }
  }

  lazy val isComment: Boolean = line.startsWith("#")

  lazy val ignore: Boolean = isComment || featureType == Ignore

  lazy val chr: String = tokens(0)

  lazy val source: String = tokens(1)

  lazy val featureType: FeatureType = FeatureType.forLabel(tokens(2))

  lazy val start: Int = GTF2Record.zeroBasedInclusiveStart(tokens(3).toInt)

  lazy val end: Int = GTF2Record.zeroBasedExclusiveEnd(tokens(4).toInt)

  lazy val score: Option[Float] = tokens(5) match {
    case "." => None
    case s: String => Some(s.toFloat)
  }

  lazy val orientation: Orientation = tokens(6) match {
    case "+" => Plus
    case "-" => Minus
    case _ => throw new IllegalArgumentException(s"Invalid GTF2 orientation: ${tokens(6)}")
  }

  lazy val frame: Int = tokens(7).toInt

  lazy val attributes: Map[String, String] = {
    // Map of attribute name to attribute value
    val as = new scala.collection.mutable.HashMap[String, String]
    // Attribute key/value pairs are separated by semicolons
    tokens(8).split("; ").foreach(s => {
      // Key and value are separated by a single space
      val ss: Array[String] = s.split(" ")
      if(ss.length < 2) throw new IllegalArgumentException(s"Invalid GTF2 attribute: $s\n$line")
      val k: String = ss.head
      val v: String = ss.tail.mkString(" ").replace("\"", "") // String values are between double quotes
      if(k == "") throw new IllegalArgumentException(s"Invalid GTF2 attribute: $s\n$line")
      // Skip empty string attribute values
      if(v != "") as.put(k, v)
    })
    // Convert to an immutable map
    as.toMap
  }

  lazy val transcriptId: Option[String] = attributes.get("transcript_id")

  lazy val geneId: Option[String] = attributes.get("gene_id")

  override def op(fb: FeatureBuilder): FeatureBuilder = {

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

object GTF2Record {

  def zeroBasedInclusiveStart(gtf2start: Int): Int = gtf2start - 1

  def zeroBasedExclusiveEnd(gtf2end: Int): Int = gtf2end

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

}

sealed trait FeatureType
case object CDS extends FeatureType
case object StartCodon extends FeatureType
case object StopCodon extends FeatureType
case object UTR5 extends FeatureType
case object UTR3 extends FeatureType
case object Intergenic extends FeatureType
case object IntergenicCNS extends FeatureType
case object IntronicCNS extends FeatureType
case object Exon extends FeatureType
case object Ignore extends FeatureType

object FeatureType {
  def forLabel(label: String): FeatureType = {
    label match {
      case "exon" => Exon
      case "CDS" => CDS
      case "start_codon" => StartCodon
      case "stop_codon" => StopCodon
      case "5UTR" => UTR5
      case "3UTR" => UTR3
      case "inter" => Intergenic
      case "inter_CNS" => IntergenicCNS
      case "intron_CNS" => IntronicCNS
      case _ => Ignore
    }
  }
}