import java.io.File

import feature.Minus
import htsjdk.samtools.{SAMRecord, SamReaderFactory}
import sequencing.{SamMapping, SamReader}

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


}
