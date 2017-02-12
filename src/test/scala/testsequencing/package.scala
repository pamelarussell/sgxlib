import java.io.File

import feature._
import htsjdk.samtools.SAMRecord
import sequencing.SamReader

import scala.collection.JavaConversions._

/**
  * Created by prussell on 1/27/17.
  */
package object testsequencing {

  val samReaderPaired: SamReader = new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath))

  val samReaderUnpaired: SamReader = new SamReader(new File(getClass.getResource("/DRR023752_read1_chr20_37Mb_38Mb.bam").getPath))

  val samReaderUnpairedShortRefName: SamReader = new SamReader(
    new File(getClass.getResource("/DRR023752_read1_chr20_37Mb_38Mb_shortRefName.bam").getPath))

  private def findSamRecord(reader: SamReader, readName: String, ref: String, min: Int, max: Int): SAMRecord = {
    val it = reader.makeReader.query(ref, min, max, true)
    val rtrn = asScalaIterator(it).find(_.getReadName == readName).get
    it.close()
    rtrn
  }

  val DRR0237521778081_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.1778081", "chr20", 36998512, 37004220)
  val DRR02375226601209_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.26601209", "chr20", 37018274, 37018354)
  val DRR0237521381132_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.1381132", "chr20", 37067979, 37068039)
  val DRR02375225421084_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.25421084", "chr20", 37018275, 37018355)
  val DRR02375221918054_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.21918054", "chr20", 37066529, 37067591)
  val DRR02375228168172_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.28168172", "chr20", 37067063, 37067283)
  val DRR02375227278814_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.27278814", "chr20", 37288821, 37302890)
  val DRR0237529822998_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.9822998", "chr20", 37298531, 37301584)
  val DRR02375216305998_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.16305998", "chr20", 37003545, 37003983)

  val DRR02375229686457_unpaired: SAMRecord = findSamRecord(samReaderUnpaired, "DRR023752.29686457", "chr20", 37521192, 37522969)
  val DRR0237526367658_unpaired: SAMRecord = findSamRecord(samReaderUnpaired, "DRR023752.6367658", "chr20", 37521192, 37522969)
  val DRR02375217157817_unpaired: SAMRecord = findSamRecord(samReaderUnpaired, "DRR023752.17157817", "chr20", 37521192, 37522969)

  val DRR02375229686457_unpaired_shortRefName: SAMRecord =
    findSamRecord(samReaderUnpairedShortRefName, "DRR023752.29686457", "20", 37521192, 37522969)
  val DRR0237526367658_unpaired_shortRefName: SAMRecord =
    findSamRecord(samReaderUnpairedShortRefName, "DRR023752.6367658", "20", 37521192, 37522969)
  val DRR02375217157817_unpaired_shortRefName: SAMRecord =
    findSamRecord(samReaderUnpairedShortRefName, "DRR023752.17157817", "20", 37521192, 37522969)


  val unmapped_paired: SAMRecord = DRR0237521778081_paired.deepCopy()
  unmapped_paired.setReadUnmappedFlag(true)
  val unmapped_unpaired: SAMRecord = DRR02375229686457_unpaired.deepCopy()
  unmapped_unpaired.setReadUnmappedFlag(true)

  /* For testing with constructed features

    Read name = DRR023752.7248016
    ----------------------
    Location = chr20:37,203,951
    Alignment start = 37,203,935 (-)
    Cigar = 26M806N13M
    Mapped = yes
    Mapping quality = 50
    Secondary = no
    Supplementary = no
    Duplicate = no
    Failed QC = no
    ----------------------
    Base = G
    Base phred quality = 40
    ----------------------
    Mate is mapped = yes
    Mate start = chr20:37199138 (+)
    Insert size = -5641
    First in pair
    Pair orientation = F2R1

    Zero-based half open read blocks: 37203934-37203960, 37204766-37204779

   */
  val DRR0237527248016_paired: SAMRecord = findSamRecord(samReaderPaired, "DRR023752.7248016", "chr20", 37203669, 37205116)
  // The actual blocks of the read
  val DRR0237527248016_blks: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203934, 37203960, Minus), Block("20", 37204766, 37204779, Minus))), None)
  // Features that contain the read
  val DRR0237527248016_contained1: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203933, 37203960, Minus), Block("20", 37204766, 37204779, Minus))), None)
  val DRR0237527248016_contained2: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203934, 37203960, Minus), Block("20", 37204766, 37204780, Minus))), None)
  val DRR0237527248016_contained3: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203934, 37203960, Minus), Block("20", 37204766, 37204779, Minus))), None)
  // Features that do not contain the read with compatible introns
  val DRR0237527248016_notContained1: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203934, 37203961, Minus), Block("20", 37204766, 37204779, Minus))), None)
  val DRR0237527248016_notContained2: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203934, 37203960, Minus), Block("20", 37204767, 37204779, Minus))), None)
  val DRR0237527248016_notContained3: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203935, 37203960, Minus), Block("20", 37204766, 37204779, Minus))), None)
  val DRR0237527248016_notContained4: Feature = new GenericFeature(BlockSet(List(
    Block("20", 37203934, 37203960, Minus), Block("20", 37204766, 37204778, Minus))), None)

}
