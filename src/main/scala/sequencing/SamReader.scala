package sequencing

import feature._
import htsjdk.samtools.SAMFileHeader.SortOrder
import htsjdk.samtools.{SAMRecord, SAMRecordIterator}

import scala.collection.JavaConversions._

/** A wrapper around an [[htsjdk.samtools.SamReader]].
  *
  * @param reader The [[htsjdk.samtools.SamReader]]
  */
class SamReader(val reader: htsjdk.samtools.SamReader) {

  /**
    * True if reference names all start with "chr", false if all reference names do not start with "chr",
    * throws exception otherwise
    */
  private val longRefNames: Boolean = {

    def all(b: Boolean) = {
      reader
        .getFileHeader
        .getSequenceDictionary
        .getSequences
        .forall(seq => {
          val nm = seq.getSequenceName
          if (b) nm.startsWith("chr") else !nm.startsWith("chr")
        })
    }

    if(all(true)) true
    else if(all(false)) false
    else throw new IllegalArgumentException("Either all reference names must begin with \"chr\"" +
      "or all must not begin with \"chr\"")

  }

  /** Returns a [[SAMRecordIterator]] over all records in the reader. */
  def iterator: SAMRecordIterator = reader.iterator()

  // Convert an external chromosome name to be compatible with names in this reader
  private def convertChr(chr: String): String = {
    if(longRefNames && !chr.startsWith("chr")) "chr" + chr
    else if(!longRefNames && chr.startsWith("chr")) chr.replaceFirst("chr", "")
    else chr
  }

  /** Returns an iterator over SAMRecords overlapping an interval.
    *
    * @param chr Interval reference sequence name
    * @param start Zero-based inclusive interval start position
    * @param end Zero-based exclusive interval end position
    * @param contained Fully contained records only
    * @return Iterator over overlapping records
    */
  def query(chr: String, start: Int, end: Int, contained: Boolean): SAMRecordIterator =
    reader.query(convertChr(chr), SamMapping.zeroBasedToSam(start), SamMapping.zeroBasedToSam(end), contained)


  /** Returns an iterator over SAMRecords that are compatible with the [[Feature]].
    *
    * In other words, records are fully contained in the [[Feature]], originate from fragments transcribed
    * from the same strand if strand specific, and have compatible introns as defined in [[feature.Region.containsCompatibleIntrons]].
    *
    * @param feat The [[Feature]]
    * @param firstOfPairStrandOnTranscript Strand relative to transcription strand of read 1 (if reads are paired) or
    *                                      all reads (if unpaired). If read 1 maps to the transcription strand, this parameter
    *                                      should be [[Plus]]. If read 1 maps to the opposite of the transcription strand,
    *                                      this parameter should be [[Minus]]. If reads are not strand-specific, this parameter
    *                                      should be [[Unstranded]].
    * @return Closeable iterator over SAMRecords that are compatible with the [[Feature]]
    */
  def compatibleRecords(feat: Feature, firstOfPairStrandOnTranscript: Orientation): SAMRecordIterator = {
    val iter = query(feat.getChr, feat.getStart, feat.getEnd, contained = true)
    new FilteredSamRecordIterator(iter,
       rec => feat.containsCompatibleIntrons(SamMapping(rec, firstOfPairStrandOnTranscript)))
  }


}

/** A filtered SAMRecordIterator
  *
  * @param iter SAMRecordIterator to filter
  * @param isValid Function that returns true for records to keep and false for records to remove
  */
private class FilteredSamRecordIterator(private val iter: SAMRecordIterator, val isValid: (SAMRecord => Boolean))
  extends SAMRecordIterator {

  private def findNxt: Option[SAMRecord] = {
    while(iter.hasNext) {
      val newRec = iter.next()
      if(isValid(newRec)) return Some(newRec)
    }
    None
  }

  private var nxt: Option[SAMRecord] = findNxt

  override def assertSorted(sortOrder: SortOrder): SAMRecordIterator = iter.assertSorted(sortOrder)
  override def close(): Unit = iter.close()
  override def hasNext: Boolean = nxt.isDefined && iter.hasNext

  override def next(): SAMRecord = {
    if(!hasNext) throw new IllegalStateException("No next element")
    val rtrn = nxt
    while(iter.hasNext) {
      val newRec = iter.next()
      if(isValid(newRec)) {
        nxt = Some(newRec)
        return rtrn.get
      }
    }
    nxt = None
    rtrn.get
  }

}
