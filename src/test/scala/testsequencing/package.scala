import java.io.File

import feature._
import htsjdk.samtools.{SAMRecord, SamReaderFactory}
import sequencing.SamReader

import scala.collection.JavaConversions._

/**
  * Created by prussell on 1/27/17.
  */
package object testsequencing {

  val samReader: SamReader = new SamReader(SamReaderFactory.makeDefault()
    .open(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath)))

  private def findSamRecord(readName: String, min: Int, max: Int): SAMRecord = {
    val it = samReader.query("chr20", min, max, contained = true)
    val rtrn = asScalaIterator(it).find(_.getReadName == readName).get
    it.close()
    rtrn
  }

  val DRR0237521778081: SAMRecord = findSamRecord("DRR023752.1778081", 36998512, 37004220)
  val DRR02375226601209: SAMRecord = findSamRecord("DRR023752.26601209", 37018274, 37018354)
  val DRR0237521381132: SAMRecord = findSamRecord("DRR023752.1381132", 37067979, 37068039)
  val DRR02375225421084: SAMRecord = findSamRecord("DRR023752.25421084", 37018275, 37018355)
  val DRR02375221918054: SAMRecord = findSamRecord("DRR023752.21918054", 37066529, 37067591)
  val DRR02375228168172: SAMRecord = findSamRecord("DRR023752.28168172", 37067063, 37067283)
  val DRR02375227278814: SAMRecord = findSamRecord("DRR023752.27278814", 37288821, 37302890)
  val DRR0237529822998: SAMRecord = findSamRecord("DRR023752.9822998", 37298531, 37301584)

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
  val DRR0237527248016: SAMRecord = findSamRecord("DRR023752.7248016", 37203669, 37205116)
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
