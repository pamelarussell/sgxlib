import java.io.File
import feature._
import htsjdk.samtools.SAMRecord
import sequencing.SamReader

// Construct a SamReader
val samReader: SamReader = new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath))

// Get iterator over all records
val nRec: Int = samReader.iterator.size // 45787

// Get fragments compatible with a feature
val frag: Iterator[(SAMRecord, SAMRecord)] =
  samReader.compatibleFragments(
    new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Plus)
    /*
      (DRR023752.11381138 ,DRR023752.11381138)
      (DRR023752.16260396 ,DRR023752.16260396)
      (DRR023752.25576103 ,DRR023752.25576103)
      (DRR023752.34173869 ,DRR023752.34173869)
    */

// Get count of fragments compatible with a feature
val count: Int = samReader.countCompatibleFragments(
  new GenericFeature(Block("20", 37500065, 37500486, Unstranded), None), Unstranded) // 5

/* Note: the above methods for fragments are also available for single records,
regardless of whether the library is paired end. */
