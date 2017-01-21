package sequencing

import feature._
import htsjdk.samtools.SAMRecord

import scala.collection.JavaConversions._

final case class SamMapping(record: SAMRecord, firstOfPairStrand: Orientation)
  extends GenericFeature(SamMapping.getBlocks(record, firstOfPairStrand), SamMapping.getQname(record))


object SamMapping {

  val MISSING = "*"

  def getBlocks(record: SAMRecord, firstOfPairStrand: Orientation): Region = {

    val chr = record.getReferenceName

    val strand: Orientation = {
      if(record.getFirstOfPairFlag || !record.getReadPairedFlag) firstOfPairStrand
      else Orientation.invert(firstOfPairStrand)
    }

    val blkList: List[Block] = record.getAlignmentBlocks.map(blk => {
      val start = samToZeroBased(blk.getReferenceStart)
      Block(chr, start, start + blk.getLength, strand)
    }).toList

    blkList match {
      case Nil => Empty
      case blk :: Nil => blk
      case _ => BlockSet(blkList)
    }

  }

  def getQname(record: SAMRecord): Option[String] = {
    record.getReadName match {
      case MISSING => None
      case s: String => Some(s)
    }
  }

  def samToZeroBased(samPos: Int): Int = samPos - 1

}


