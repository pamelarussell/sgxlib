package sequencing

import java.io.File

import feature._
import htsjdk.samtools.SAMFileHeader.SortOrder
import htsjdk.samtools.{SAMRecord, SAMRecordIterator, SamReaderFactory}

import scala.collection.JavaConversions._

/** A wrapper around an [[htsjdk.samtools.SamReader]].
  *
  * @param file the BAM file
  */
class SamReader(private val file: File) {

  /** Returns a [[htsjdk.samtools.SamReader]] reading from the same file as this object.
    *
    * Use this if you want the additional functionality provided by that class.
    * But beware, [[htsjdk.samtools.SamReader]] objects are mutable, and iterators over
    * them must be closed.
    */
  def makeReader: htsjdk.samtools.SamReader = SamReaderFactory.makeDefault().open(file)

  /**
    * True if reference names all start with "chr", false if all reference names do not start with "chr",
    * throws exception otherwise
    */
  private val longRefNames: Boolean = {

    def all(b: Boolean) = {
      makeReader
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

  /** Returns an iterator over all records in the reader. */
  def iterator: Iterator[SAMRecord] = new FilteredSamRecordIterator(makeReader.iterator(), _ => true)

  // Convert an external chromosome name to be compatible with names in this reader
  private def convertChr(chr: String): String = {
    if(longRefNames && !chr.startsWith("chr")) "chr" + chr
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
  private def query(chr: String, start: Int, end: Int, contained: Boolean): SAMRecordIterator =
    makeReader.query(convertChr(chr), SamMapping.zeroBasedToSam(start), SamMapping.zeroBasedToSam(end), contained)


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
  def compatibleRecords(feat: Feature, firstOfPairStrandOnTranscript: Orientation): Iterator[SAMRecord] = {
    val iter = query(feat.getChr, feat.getStart, feat.getEnd, contained = true)
    new FilteredSamRecordIterator(iter,
       rec => feat.containsCompatibleIntrons(SamMapping(rec, firstOfPairStrandOnTranscript)))
  }

  /** Returns the number of SAMRecords that are compatible with the [[Feature]].
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
  def countCompatibleRecords(feat: Feature, firstOfPairStrandOnTranscript: Orientation): Int = {
    compatibleRecords(feat, firstOfPairStrandOnTranscript).size
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
  override def hasNext: Boolean = nxt.isDefined

  override def next(): SAMRecord = {
    if(!hasNext) throw new NoSuchElementException("No next element")
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
