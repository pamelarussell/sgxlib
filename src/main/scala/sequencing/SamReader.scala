package sequencing

import java.io.File

import feature._
import htsjdk.samtools.SAMFileHeader.SortOrder
import htsjdk.samtools.{SAMRecord, SAMRecordIterator, SamReaderFactory}

import scala.collection.JavaConversions._

/** A wrapper around an [[htsjdk.samtools.SamReader]].
  *
  * @param file the BAM file
  * @param isValid Filter to apply to all SAMRecords in this reader.
  *                SAMRecords are returned only if they evaluate to true under this function.
  */
class SamReader(private val file: File, val isValid: SAMRecord => Boolean = _ => true) {

  /** Returns a [[htsjdk.samtools.SamReader]] reading from the same file as this object.
    *
    * Use this if you want the additional functionality provided by that class.
    * But beware, [[htsjdk.samtools.SamReader]] objects are mutable. Also, these
    * objects and iterators over them must be closed.
    */
  def makeReader: htsjdk.samtools.SamReader = SamReaderFactory.makeDefault().open(file)

  /**
    * True if the first record is paired; false if it is unpaired or the reader is empty
    *
    * This value is used to assert this status for all reads in the reader - no mixing of
    * paired and unpaired reads is allowed.
    */
  private val paired: Boolean = {
    val reader = makeReader
    val it = reader.iterator()
    val rtrn = {
      if (it.hasNext) it.next().getReadPairedFlag
      else false
    }
    it.close()
    reader.close()
    rtrn
  }

  /**
    * True if reference names all start with "chr", false if all reference names do not start with "chr",
    * throws exception otherwise
    */
  private val longRefNames: Boolean = {

    def all(b: Boolean) = {
      val reader = makeReader
      val rtrn = reader
        .getFileHeader
        .getSequenceDictionary
        .getSequences
        .forall(seq => {
          val nm = seq.getSequenceName
          if (b) nm.startsWith("chr") else !nm.startsWith("chr")
        })
      reader.close()
      rtrn
    }

    if(all(true)) true
    else if(all(false)) false
    else throw new IllegalArgumentException("Either all reference names must begin with \"chr\"" +
      "or all must not begin with \"chr\"")

  }

  /** Returns an iterator over all records in the reader evaluating to true under [[isValid]]. */
  def iterator: Iterator[SAMRecord] = {
    val reader = makeReader
    new FilteredSamRecordIterator(reader, reader.iterator(), isValid)
  }

  // Convert an external chromosome name to be compatible with names in this reader
  private def convertChr(chr: String): String = {
    if(longRefNames && !chr.startsWith("chr")) "chr" + chr
    else chr
  }

  /** Returns an iterator over SAMRecords overlapping an interval, along with a pointer
    * to the [[htsjdk.samtools.SamReader]] it iterates over.
    *
    * Only returns records evaluating to true under [[isValid]].
    *
    * @param chr Interval reference sequence name
    * @param start Zero-based inclusive interval start position
    * @param end Zero-based exclusive interval end position
    * @param contained Fully contained records only
    * @return Pair of the implicated [[htsjdk.samtools.SamReader]] and the iterator over overlapping records.
    *         The reader and iterator need to be closed.
    */
  private def query(chr: String, start: Int, end: Int, contained: Boolean): (htsjdk.samtools.SamReader, SAMRecordIterator) = {
    val reader = makeReader
    (reader, reader.query(convertChr(chr), SamMapping.zeroBasedToSam(start), SamMapping.zeroBasedToSam(end), contained))
  }


  /** Returns an iterator over SAMRecords that are compatible with the [[Feature]].
    *
    * In other words, records are fully contained in the [[Feature]], originate from fragments transcribed
    * from the same strand if strand specific, and have compatible introns as defined in [[feature.Region.containsCompatibleIntrons]].
    *
    * Only returns records evaluating to true under [[isValid]].
    *
    * @param feat The [[Feature]]
    * @param firstOfPairStrandOnTranscript Strand relative to transcription strand of read 1 (if reads are paired) or
    *                                      all reads (if unpaired). If read 1 maps to the transcription strand, this parameter
    *                                      should be [[Plus]]. If read 1 maps to the opposite of the transcription strand,
    *                                      this parameter should be [[Minus]]. If reads are not strand-specific, this parameter
    *                                      should be [[Unstranded]].
    * @return Iterator over SAMRecords that are compatible with the [[Feature]]
    */
  def compatibleRecords(feat: Feature, firstOfPairStrandOnTranscript: Orientation): Iterator[SAMRecord] = {
    val q: (htsjdk.samtools.SamReader, SAMRecordIterator) = query(feat.getChr, feat.getStart, feat.getEnd, contained = true)
    new FilteredSamRecordIterator(q._1, q._2,
       rec => isValid(rec) && feat.containsCompatibleIntrons(SamMapping(rec, firstOfPairStrandOnTranscript)))
  }

  /** Returns the number of SAMRecords that are compatible with the [[Feature]].
    *
    * In other words, records are fully contained in the [[Feature]], originate from fragments transcribed
    * from the same strand if strand specific, and have compatible introns as defined in [[feature.Region.containsCompatibleIntrons]].
    *
    * Only counts records evaluating to true under [[isValid]].
    *
    * @param feat The [[Feature]]
    * @param firstOfPairStrandOnTranscript Strand relative to transcription strand of read 1 (if reads are paired) or
    *                                      all reads (if unpaired). If read 1 maps to the transcription strand, this parameter
    *                                      should be [[Plus]]. If read 1 maps to the opposite of the transcription strand,
    *                                      this parameter should be [[Minus]]. If reads are not strand-specific, this parameter
    *                                      should be [[Unstranded]].
    * @return Number of SAMRecords that are compatible with the [[Feature]]
    */
  def countCompatibleRecords(feat: Feature, firstOfPairStrandOnTranscript: Orientation): Int = {
    compatibleRecords(feat, firstOfPairStrandOnTranscript).size
  }

  /** Returns an iterator over pairs of SAMRecords that are compatible with the [[Feature]].
    *
    * Each returned pair is a mate pair and is in the order (first of pair, second of pair).
    *
    * Returned records are fully contained in the [[Feature]], originate from fragments transcribed
    * from the same strand if strand specific, and have compatible introns as defined in [[feature.Region.containsCompatibleIntrons]].
    *
    * Only mate pairs with both mates compatible with the [[Feature]] and evaluating to true under [[isValid]]
    * are returned. Only primary alignments are used.
    *
    * The returned iterator is in no particular order.
    *
    * Throws [[IllegalArgumentException]] if records are not paired.
    *
    * @param feat The [[Feature]]
    * @param firstOfPairStrandOnTranscript Strand relative to transcription strand of read 1 (if reads are paired) or
    *                                      all reads (if unpaired). If read 1 maps to the transcription strand, this parameter
    *                                      should be [[Plus]]. If read 1 maps to the opposite of the transcription strand,
    *                                      this parameter should be [[Minus]]. If reads are not strand-specific, this parameter
    *                                      should be [[Unstranded]].
    * @return
    */
  def compatibleFragments(feat: Feature, firstOfPairStrandOnTranscript: Orientation): Iterator[(SAMRecord, SAMRecord)] = {
    val q: (htsjdk.samtools.SamReader, SAMRecordIterator) = query(feat.getChr, feat.getStart, feat.getEnd, contained = true)
    new PairedIterator(
      q._1,
      new FilteredSamRecordIterator(
        q._1, q._2,
        rec => isValid(rec)
          && !rec.getNotPrimaryAlignmentFlag
          && feat.containsCompatibleIntrons(SamMapping(rec, firstOfPairStrandOnTranscript))))
  }


  /** Returns the number of pairs of SAMRecords that are compatible with the [[Feature]].
    *
    * Included records are fully contained in the [[Feature]], originate from fragments transcribed
    * from the same strand if strand specific, and have compatible introns as defined in [[feature.Region.containsCompatibleIntrons]].
    *
    * Only mate pairs with both mates compatible with the [[Feature]] and evaluating to true under [[isValid]]
    * are included. Only primary alignments are used.
    *
    * Throws [[IllegalArgumentException]] if records are not paired.
    *
    * @param feat The [[Feature]]
    * @param firstOfPairStrandOnTranscript Strand relative to transcription strand of read 1 (if reads are paired) or
    *                                      all reads (if unpaired). If read 1 maps to the transcription strand, this parameter
    *                                      should be [[Plus]]. If read 1 maps to the opposite of the transcription strand,
    *                                      this parameter should be [[Minus]]. If reads are not strand-specific, this parameter
    *                                      should be [[Unstranded]].
    * @return
    */
  def countCompatibleFragments(feat: Feature, firstOfPairStrandOnTranscript: Orientation): Int =
    compatibleFragments(feat, firstOfPairStrandOnTranscript).size


  /** Iterator over mate pairs contained in the provided iterator.
    *
    * Pairs returned by this iterator are in the order (first of pair, second of pair).
    *
    * Only mates with both pairs in the iterator are included.
    *
    * @param reader The implicated [[htsjdk.samtools.SamReader]]
    * @param iter SAMRecordIterator over the reader
    */
  private final class PairedIterator(private val reader: htsjdk.samtools.SamReader,
                                     private val iter: FilteredSamRecordIterator) extends Iterator[(SAMRecord, SAMRecord)] {

    if(!paired) throw new IllegalArgumentException("Invalid call for unpaired reads")
    iter.assertSorted(SortOrder.coordinate)

    // Records that have been seen and mate will potentially still be seen
    private val waitingForMate: scala.collection.mutable.HashMap[String, SAMRecord] = scala.collection.mutable.HashMap()

    private var nxt: Option[(SAMRecord, SAMRecord)] = findNxt

    // True if mate might still be out there
    private def waitForMate(rec: SAMRecord): Boolean = {
      if(rec.getMateUnmappedFlag) false
      else if(rec.getReadNegativeStrandFlag == rec.getMateNegativeStrandFlag) false
      else rec.getAlignmentStart <= rec.getMateAlignmentStart
    }

    private def findNxt: Option[(SAMRecord, SAMRecord)] = {
      if(!iter.hasNext) {
        iter.close()
        reader.close()
        None
      } else {
        while(iter.hasNext) {
          val rec = iter.next()
          val name = rec.getReadName
          if(waitingForMate.contains(name)) {
            if(rec.getFirstOfPairFlag) return Some((rec, waitingForMate(name)))
            else return Some(waitingForMate(name), rec)
          } else if(waitForMate(rec)) {
            waitingForMate.put(name, rec)
          }
        }
        iter.close()
        reader.close()
      }
      iter.close()
      reader.close()
      None
    }

    override def hasNext: Boolean = nxt.isDefined

    override def next(): (SAMRecord, SAMRecord) = {
      if(!hasNext) throw new NoSuchElementException("No next element")
      val rtrn = nxt.get
      nxt = findNxt
      rtrn
    }

  }

  /** A filtered SAMRecordIterator
    *
    * @param reader The implicated [[htsjdk.samtools.SamReader]]
    * @param iter SAMRecordIterator to filter
    * @param keepRecord Function that returns true for records to keep and false for records to remove
    */
  private final class FilteredSamRecordIterator(private val reader: htsjdk.samtools.SamReader,
                                                private val iter: SAMRecordIterator,
                                                val keepRecord: (SAMRecord => Boolean))
    extends SAMRecordIterator {

    private def findNxt: Option[SAMRecord] = {
      while(iter.hasNext) {
        val newRec = iter.next()
        if(newRec.getReadPairedFlag != paired)
          throw new IllegalArgumentException("Cannot mix paired and unpaired reads")
        if(keepRecord(newRec)) return Some(newRec)
      }
      iter.close()
      reader.close()
      None
    }

    private var nxt: Option[SAMRecord] = findNxt

    override def assertSorted(sortOrder: SortOrder): SAMRecordIterator = iter.assertSorted(sortOrder)
    override def close(): Unit = {
      iter.close()
      reader.close()
    }
    override def hasNext: Boolean = nxt.isDefined

    override def next(): SAMRecord = {
      if(!hasNext) throw new NoSuchElementException("No next element")
      val rtrn = nxt.get
      nxt = findNxt
      rtrn
    }

  }


}

