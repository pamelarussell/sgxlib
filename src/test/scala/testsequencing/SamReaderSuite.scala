package testsequencing

import java.io.File

import feature._
import org.scalatest.FunSuite
import sequencing.SamReader
import shared.GTF22Data._

/**
  * Created by prussell on 2/8/17.
  */
class SamReaderSuite extends FunSuite {

  test("Iterator size") {
    assert(samReaderPaired.iterator.size === 45787)
    assert(samReaderPaired.compatibleRecords(ENST00000411780, Minus).size === 5)
    assert(samReaderPaired.compatibleRecords(ENST00000411780, Unstranded).size === 5)
    assert(samReaderPaired.compatibleRecords(ENST00000411780, Plus).size === 0)
  }

  test("Iterator size - reader with filter") {
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => rec.getNotPrimaryAlignmentFlag && rec.getReadPairedFlag)
      .iterator.size === 1024)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => !rec.getReadPairedFlag)
      .iterator.size === 0)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => rec.getReadPairedFlag)
      .iterator.size === 45787)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => rec.getReadNegativeStrandFlag).compatibleRecords(ENST00000411780, Minus).size === 3)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => !rec.getReadNegativeStrandFlag).compatibleRecords(ENST00000411780, Minus).size === 2)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => !rec.getReadNegativeStrandFlag).compatibleRecords(ENST00000411780, Plus).size === 0)
  }

  test("Count compatible records") {
    assert(samReaderPaired.countCompatibleRecords(ENST00000411780, Minus) === 5)
    assert(samReaderPaired.countCompatibleRecords(ENST00000411780, Unstranded) === 5)
    assert(samReaderPaired.countCompatibleRecords(ENST00000411780, Plus) === 0)
  }

  test("Iterator size - count compatible records with filter") {
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => rec.getReadNegativeStrandFlag).countCompatibleRecords(ENST00000411780, Minus) === 3)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => !rec.getReadNegativeStrandFlag).countCompatibleRecords(ENST00000411780, Minus) === 2)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      rec => !rec.getReadNegativeStrandFlag).countCompatibleRecords(ENST00000411780, Plus) === 0)
  }

  test("No next element entire file") {
    val iter1 = samReaderPaired.iterator
    while(iter1.hasNext) {iter1.next()}
    intercept[NoSuchElementException]{iter1.next()}
  }

  test("No next element one feature") {
    val iter2 = samReaderPaired.compatibleRecords(ENST00000525052, Plus)
    while(iter2.hasNext) {iter2.next()}
    intercept[NoSuchElementException]{iter2.next()}
  }

  test("Invalid header") {
    intercept[IllegalArgumentException]{new SamReader(new File(getClass.getResource("/invalid_header.bam").getPath))}
  }

  test("Invalid mixed paired and unpaired bam") {
    val sr = new SamReader(new File(getClass.getResource("/mixed_paired_unpaired.bam").getPath))
    val it = sr.iterator
    intercept[IllegalArgumentException] {
      while(it.hasNext) it.next()
    }
    intercept[IllegalArgumentException] {
      sr.countCompatibleRecords(new GenericFeature(Block("20", 37499114, 37510351, Minus), None), Plus)
    }
  }

}
