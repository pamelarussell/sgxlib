package sequencing

import feature._
import htsjdk.samtools.SAMRecord

import scala.collection.JavaConversions._

/** A [[Feature]] representing a mapped SAM record.
  *
  * The [[Block]]s of this [[SamMapping]] are the aligned blocks of the SAM record translated to zero-based
  * half-open intervals.
  *
  * If the sequencing library is strand specific, the [[Orientation]] of this [[Feature]] is the transcription
  * strand of the entire fragment the record came from. For example, if the reads are unpaired and strand-specific,
  * and always map to the transcription strand for this library, then this [[SamMapping]] will have [[Orientation]] [[Plus]] for a read
  * mapped to the plus strand or [[Minus]] for a read mapped to the minus strand. If the reads are paired and read 1
  * always gives the transcription strand for this library, and we have a pair of records with read 1 mapped to the
  * plus strand and read 2 mapped to the minus strand, then the entire fragment was transcribed from the plus strand,
  * so the [[Orientation]] of both [[SamMapping]]s is [[Plus]].
  *
  * If the library is not RNA-seq or is not strand specific, the [[Orientation]] is [[Unstranded]].
  *
  * @param record The SAM record
  * @param firstOfPairStrandOnTranscript Strand relative to transcription strand of read 1 (if reads are paired) or
  *                                      all reads (if unpaired). If read 1 maps to the transcription strand, this parameter
  *                                      should be [[Plus]]. If read 1 maps to the opposite of the transcription strand,
  *                                      this parameter should be [[Minus]]. If reads are not strand-specific, this parameter
  *                                      should be [[Unstranded]].
  */
final case class SamMapping(record: SAMRecord, firstOfPairStrandOnTranscript: Orientation)
  extends GenericFeature(SamMapping.getBlocks(record, firstOfPairStrandOnTranscript), SamMapping.getQname(record)) {

  validate()

  private def validate(): Unit = {
    if(record.getReadUnmappedFlag) throw new IllegalArgumentException("Cannot create SamMapping from unmapped read: " + record.getSAMString)
  }

}


object SamMapping {

  val MISSING = "*"

  // Get the blocks of the SAMRecord as a Region
  protected def getBlocks(record: SAMRecord, firstOfPairStrandOnTranscript: Orientation): Region = {

    val chr = record.getReferenceName.replaceFirst("^chr", "")

    val strand: Orientation = {
      if(firstOfPairStrandOnTranscript == Unstranded) Unstranded
      else {
        val mappedStrand: Orientation = if (record.getReadNegativeStrandFlag) Minus else Plus
        firstOfPairStrandOnTranscript match {
          case Plus =>
            if(!record.getReadPairedFlag || record.getFirstOfPairFlag) mappedStrand else Orientation.invert(mappedStrand)
          case Minus =>
            if(!record.getReadPairedFlag || record.getFirstOfPairFlag) Orientation.invert(mappedStrand) else mappedStrand
          case Unstranded => Unstranded
        }
      }
    }


    // Make the list of blocks
    val blkList: List[Block] = record.getAlignmentBlocks.map(blk => {
      val start = samToZeroBased(blk.getReferenceStart)
      Block(chr, start, start + blk.getLength, strand)
    }).toList
      .foldLeft(List[Block]()) { // Combine adjacent (touching) blocks surrounding an insertion
        case (head :: tail, blk) if head.end == blk.start =>
          Block(head.chr, head.start, blk.end, head.orientation) :: tail
        case (other, blk) =>
          blk :: other
      }.reverse

    // Return appropriate Region type
    blkList match {
      case Nil => Empty
      case blk :: Nil => blk
      case _ =>
        try {
          BlockSet(blkList)
        } catch {
          case e: Throwable =>
            println("Exception on record:\n" + record.getSAMString)
            throw e
        }
    }

  }

  // Convert query name to an Option
  protected def getQname(record: SAMRecord): Option[String] = {
    record.getReadName match {
      case MISSING => None
      case s: String => Some(s)
    }
  }

  /** Returns a zero-based coordinate converted from a SAM record coordinate
    *
    * @param samPos Position from a SAM record
    * @return Corresponding zero-based position
    */
  def samToZeroBased(samPos: Int): Int = samPos - 1

  /** Returns a SAM record coordinate converted from a zero-based position
    *
    * @param zeroBasedPos Zero-based position
    * @return Corresponding SAM record position
    */
  def zeroBasedToSam(zeroBasedPos: Int): Int = zeroBasedPos + 1

}


