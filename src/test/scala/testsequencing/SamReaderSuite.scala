package testsequencing

import java.io.File

import feature._
import htsjdk.samtools.SAMRecord
import org.scalatest.FunSuite
import sequencing.SamReader
import shared.GTF22Data._

/**
  * Created by prussell on 2/8/17.
  */
class SamReaderSuite extends FunSuite {

  def makeRecordIter(reader: SamReader, feat: Feature): Iterator[SAMRecord] =
    reader.compatibleRecords(feat)

  test("Compatible records overlapping transcript - reads paired") {

    // Read contains multiple blocks fully contained in exon
    assert(!makeRecordIter(samReaderPairedPlus, ENST00000373664).contains(DRR02375226601209_paired))
    assert(!makeRecordIter(samReaderPairedMinus, ENST00000373664).contains(DRR02375226601209_paired))

    // Mate unmapped
    assert(makeRecordIter(samReaderPairedPlus, ENST00000525052).contains(DRR0237521381132_paired))
    assert(!makeRecordIter(samReaderPairedMinus, ENST00000525052).contains(DRR0237521381132_paired))

    // Unstranded should add up to Plus + Minus
    val s8 = makeRecordIter(samReaderPairedPlus, ENST00000525052).size
    val s9 = makeRecordIter(samReaderPairedMinus, ENST00000525052).size
    val s10 = makeRecordIter(samReaderPairedUnstranded, ENST00000525052).size
    assert(s10 == s8 + s9)

    // Read 1 minus strand
    assert(makeRecordIter(samReaderPairedPlus, ENST00000373664).contains(DRR0237521778081_paired))
    assert(!makeRecordIter(samReaderPairedMinus, ENST00000373664).contains(DRR0237521778081_paired))

    // Read 1 plus strand
    assert(!makeRecordIter(samReaderPairedPlus, ENST00000373664).contains(DRR02375225421084_paired))
    assert(makeRecordIter(samReaderPairedMinus, ENST00000373664).contains(DRR02375225421084_paired))

    // Read 2 minus strand
    assert(!makeRecordIter(samReaderPairedPlus, ENST00000373664).contains(DRR02375221918054_paired))
    assert(makeRecordIter(samReaderPairedMinus, ENST00000373664).contains(DRR02375221918054_paired))

    // Read 2 plus strand
    assert(makeRecordIter(samReaderPairedPlus, ENST00000373664).contains(DRR02375228168172_paired))
    assert(!makeRecordIter(samReaderPairedMinus, ENST00000373664).contains(DRR02375228168172_paired))

    // Multiple isoforms
    assert(makeRecordIter(samReaderPairedMinus, ENST00000373606).contains(DRR02375227278814_paired))
    assert(!makeRecordIter(samReaderPairedMinus, ENST00000397152).contains(DRR02375227278814_paired))
    assert(!makeRecordIter(samReaderPairedMinus, ENST00000373606).contains(DRR0237529822998_paired))
    assert(makeRecordIter(samReaderPairedMinus, ENST00000397152).contains(DRR0237529822998_paired))

    // Constructed features for testing off by one errors
    assert(makeRecordIter(samReaderPairedPlus, DRR0237527248016_contained1).contains(DRR0237527248016_paired))
    assert(makeRecordIter(samReaderPairedPlus, DRR0237527248016_contained2).contains(DRR0237527248016_paired))
    assert(makeRecordIter(samReaderPairedPlus, DRR0237527248016_contained3).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPairedPlus, DRR0237527248016_notContained1).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPairedPlus, DRR0237527248016_notContained2).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPairedPlus, DRR0237527248016_notContained3).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPairedPlus, DRR0237527248016_notContained4).contains(DRR0237527248016_paired))

  }

  test("Compatible records overlapping transcript - reads unpaired") {

    // DRR02375229686457_unpaired
    assert(makeRecordIter(samReaderUnpairedPlus, ENST00000346199).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpairedPlus, ENST00000445723).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpairedMinus, ENST00000346199).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpairedMinus, ENST00000445723).contains(DRR02375229686457_unpaired))
    assert(makeRecordIter(samReaderUnpairedUnstranded, ENST00000346199).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpairedUnstranded, ENST00000445723).contains(DRR02375229686457_unpaired))

    // DRR0237526367658_unpaired
    assert(!makeRecordIter(samReaderUnpairedPlus, ENST00000346199).contains(DRR0237526367658_unpaired))
    assert(!makeRecordIter(samReaderUnpairedPlus, ENST00000445723).contains(DRR0237526367658_unpaired))
    assert(makeRecordIter(samReaderUnpairedMinus, ENST00000346199).contains(DRR0237526367658_unpaired))
    assert(!makeRecordIter(samReaderUnpairedMinus, ENST00000445723).contains(DRR0237526367658_unpaired))
    assert(makeRecordIter(samReaderUnpairedUnstranded, ENST00000346199).contains(DRR0237526367658_unpaired))
    assert(!makeRecordIter(samReaderUnpairedUnstranded, ENST00000445723).contains(DRR0237526367658_unpaired))

    // DRR02375217157817_unpaired
    assert(!makeRecordIter(samReaderUnpairedPlus, ENST00000346199).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpairedPlus, ENST00000445723).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpairedMinus, ENST00000346199).contains(DRR02375217157817_unpaired))
    assert(!makeRecordIter(samReaderUnpairedMinus, ENST00000445723).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpairedUnstranded, ENST00000346199).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpairedUnstranded, ENST00000445723).contains(DRR02375217157817_unpaired))

  }

  test("Compatible records overlapping transcript - reads unpaired, short reference names") {

    // Block with illegal long reference name
    intercept[IllegalArgumentException]{makeRecordIter(samReaderUnpairedShortRefNamePlus,
      new GenericFeature(Block("chr20", 37682448, 37682602, Plus), None)).size === 9}

    // DRR02375229686457_unpaired
    assert(makeRecordIter(samReaderUnpairedShortRefNamePlus, ENST00000346199).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNamePlus, ENST00000445723).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNameMinus, ENST00000346199).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNameMinus, ENST00000445723).contains(DRR02375229686457_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNameUnstranded, ENST00000346199).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNameUnstranded, ENST00000445723).contains(DRR02375229686457_unpaired_shortRefName))

    // DRR0237526367658_unpaired
    assert(!makeRecordIter(samReaderUnpairedShortRefNamePlus, ENST00000346199).contains(DRR0237526367658_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNamePlus, ENST00000445723).contains(DRR0237526367658_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNameMinus, ENST00000346199).contains(DRR0237526367658_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNameMinus, ENST00000445723).contains(DRR0237526367658_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNameUnstranded, ENST00000346199).contains(DRR0237526367658_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNameUnstranded, ENST00000445723).contains(DRR0237526367658_unpaired_shortRefName))

    // DRR02375217157817_unpaired
    assert(!makeRecordIter(samReaderUnpairedShortRefNamePlus, ENST00000346199).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNamePlus, ENST00000445723).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNameMinus, ENST00000346199).contains(DRR02375217157817_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefNameMinus, ENST00000445723).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNameUnstranded, ENST00000346199).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefNameUnstranded, ENST00000445723).contains(DRR02375217157817_unpaired_shortRefName))

  }

  test("Iterator size") {
    assert(samReaderPairedUnstranded.iterator.size === 45787)
    assert(samReaderPairedPlus.iterator.size === 45787)
    assert(samReaderPairedMinus.compatibleRecords(ENST00000411780).size === 5)
    assert(samReaderPairedUnstranded.compatibleRecords(ENST00000411780).size === 5)
    assert(samReaderPairedPlus.compatibleRecords(ENST00000411780).size === 0)
  }

  test("Iterator size - reader with filter") {
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Unstranded, rec => rec.getNotPrimaryAlignmentFlag && rec.getReadPairedFlag)
      .iterator.size === 1024)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Minus, rec => !rec.getReadPairedFlag)
      .iterator.size === 0)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Plus, rec => rec.getReadPairedFlag)
      .iterator.size === 45787)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Minus, rec => rec.getReadNegativeStrandFlag).compatibleRecords(ENST00000411780).size === 3)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Minus, rec => !rec.getReadNegativeStrandFlag).compatibleRecords(ENST00000411780).size === 2)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Plus, rec => !rec.getReadNegativeStrandFlag).compatibleRecords(ENST00000411780).size === 0)
  }

  test("Count compatible records") {
    assert(samReaderPairedMinus.countCompatibleRecords(ENST00000411780) === 5)
    assert(samReaderPairedUnstranded.countCompatibleRecords(ENST00000411780) === 5)
    assert(samReaderPairedPlus.countCompatibleRecords(ENST00000411780) === 0)
  }

  test("Count compatible records and fragments - nonexistent chromosome") {
    assert(samReaderUnpairedPlus.countCompatibleRecords(new GenericFeature(Block("xxx", 1000, 2000, Plus), None)) === 0)
    assert(samReaderPairedPlus.countCompatibleRecords(new GenericFeature(Block("xxx", 1000, 2000, Plus), None)) === 0)
    assert(samReaderUnpairedPlus.countCompatibleFragments(new GenericFeature(Block("xxx", 1000, 2000, Plus), None)) === 0)
    assert(samReaderPairedPlus.countCompatibleFragments(new GenericFeature(Block("xxx", 1000, 2000, Plus), None)) === 0)
  }

  test("Iterator size - count compatible records with filter") {
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Minus, rec => rec.getReadNegativeStrandFlag).countCompatibleRecords(ENST00000411780) === 3)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Minus, rec => !rec.getReadNegativeStrandFlag).countCompatibleRecords(ENST00000411780) === 2)
    assert(new SamReader(new File(getClass.getResource("/DRR023752_chr20_37Mb_38Mb.bam").getPath),
      Plus, rec => !rec.getReadNegativeStrandFlag).countCompatibleRecords(ENST00000411780) === 0)
  }

  test("No next element entire file") {
    val iter1 = samReaderPairedUnstranded.iterator
    while(iter1.hasNext) {iter1.next()}
    intercept[NoSuchElementException]{iter1.next()}
  }

  test("No next element one feature") {
    val iter2 = samReaderPairedPlus.compatibleRecords(ENST00000525052)
    while(iter2.hasNext) {iter2.next()}
    intercept[NoSuchElementException]{iter2.next()}
  }

  test("Invalid header") {
    intercept[IllegalArgumentException]{new SamReader(new File(getClass.getResource("/invalid_header.bam").getPath), Plus)}
  }

  test("Invalid mixed paired and unpaired bam") {
    val sr = new SamReader(new File(getClass.getResource("/mixed_paired_unpaired.bam").getPath), Plus)
    val it = sr.iterator
    intercept[IllegalArgumentException] {
      while(it.hasNext) it.next()
    }
    intercept[IllegalArgumentException] {
      sr.countCompatibleRecords(new GenericFeature(Block("20", 37499114, 37510351, Minus), None))
    }
  }

  def applySamReaderPaired(fopStrand: Orientation, fn: SamReader => Boolean): Boolean = {
    fopStrand match {
      case Plus => fn(samReaderPairedPlus)
      case Minus => fn(samReaderPairedMinus)
      case Unstranded => fn(samReaderPairedUnstranded)
    }
  }

  def existsRecord(start: Int, end: Int, orientation: Orientation, name: String, fopStrand: Orientation): Boolean = {
    def fn (samReader: SamReader) = {
      samReader.compatibleRecords(new GenericFeature(Block("20", start, end, orientation), None))
        .exists(rec => rec.getReadName == name)
    }
    applySamReaderPaired(fopStrand, fn)
  }

  def existsFragment(feat: Feature, name: String, fopStrand: Orientation): Boolean = {
    def fn (samReader: SamReader) = {
      samReader.compatibleFragments(feat)
        .exists(pair => pair._1.getReadName == name && pair._2.getReadName == name
          && pair._1.getFirstOfPairFlag && !pair._2.getFirstOfPairFlag)  }
    applySamReaderPaired(fopStrand, fn)
  }


  def notExistsFragment(feat: Feature, name: String, fopStrand: Orientation): Boolean = {
    def fn(samReader: SamReader) = {
      !samReader.compatibleFragments(feat)
        .exists(pair => pair._1.getReadName == name || pair._2.getReadName == name)  }
    applySamReaderPaired(fopStrand, fn)
    }

  test("Compatible fragments") {
    assert(samReaderPairedPlus.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)).size === 4)
    // Unpaired
    intercept[IllegalArgumentException](samReaderUnpairedPlus.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)))
    assert(samReaderPairedMinus.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)).size === 1)
    assert(samReaderPairedUnstranded.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)).size === 5)
    assert(existsFragment(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), "DRR023752.16260396", Plus))
    assert(existsFragment(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), "DRR023752.17105240", Minus))
    // No full pairs
    assert(samReaderPairedPlus.compatibleFragments(ENST00000373508).size === 0)
    assert(samReaderPairedMinus.compatibleFragments(ENST00000373508).size === 0)
    // Mate unmapped
    assert(notExistsFragment(new GenericFeature(Block("20", 37003677, 37003895, Unstranded), None),
      "DRR023752.7710123", Unstranded))
    assert(existsRecord(37003677, 37003895, Unstranded, "DRR023752.7710123", Unstranded))
    // Both mates mapped on same strand
    assert(notExistsFragment(new GenericFeature(Block("20", 37184083, 37184521, Unstranded), None),
      "DRR023752.35442471", Unstranded))
    assert(existsRecord(37184083, 37184521, Unstranded, "DRR023752.35442471", Unstranded))
    // Spliced records
    assert(existsFragment(ENST00000397152, "DRR023752.36362659", Minus))
    assert(notExistsFragment(ENST00000397152, "DRR023752.36362659", Plus))
    // Not primary alignment
    assert(notExistsFragment(new GenericFeature(Block("20", 37240662, 37240966, Unstranded), None), "DRR023752.35525881", Unstranded))
    assert(existsRecord(37240662, 37240966, Unstranded, "DRR023752.35525881", Unstranded))
  }

  test("Count compatible fragments - reads unpaired") {
    assert(samReaderUnpairedMinus.countCompatibleFragments(ENST00000411780) === 2)
    assert(samReaderUnpairedUnstranded.countCompatibleFragments(ENST00000411780) === 2)
    assert(samReaderUnpairedPlus.countCompatibleFragments(ENST00000411780) === 0)
  }

  test("Count compatible fragments - reads paired") {
    assert(samReaderPairedPlus.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)) === 4)
    assert(samReaderPairedMinus.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)) === 1)
    assert(samReaderPairedUnstranded.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None)) === 5)
    assert(samReaderPairedUnstranded.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Unstranded), None)) === 5)
    // No full pairs
    assert(samReaderPairedPlus.countCompatibleFragments(ENST00000373508) === 0)
    assert(samReaderPairedMinus.countCompatibleFragments(ENST00000373508) === 0)
  }

}
