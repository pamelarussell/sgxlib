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

  def makeRecordIter(reader: SamReader, feat: Feature, firstOfPairStrandOnTranscript: Orientation): Iterator[SAMRecord] =
    reader.compatibleRecords(feat, firstOfPairStrandOnTranscript)

  test("Compatible records overlapping transcript - reads paired") {

    // Read contains multiple blocks fully contained in exon
    assert(!makeRecordIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375226601209_paired))
    assert(!makeRecordIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375226601209_paired))

    // Mate unmapped
    assert(makeRecordIter(samReaderPaired, ENST00000525052, Plus).contains(DRR0237521381132_paired))
    assert(!makeRecordIter(samReaderPaired, ENST00000525052, Minus).contains(DRR0237521381132_paired))

    // Unstranded should add up to Plus + Minus
    val s8 = makeRecordIter(samReaderPaired, ENST00000525052, Plus).size
    val s9 = makeRecordIter(samReaderPaired, ENST00000525052, Minus).size
    val s10 = makeRecordIter(samReaderPaired, ENST00000525052, Unstranded).size
    assert(s10 == s8 + s9)

    // Read 1 minus strand
    assert(makeRecordIter(samReaderPaired, ENST00000373664, Plus).contains(DRR0237521778081_paired))
    assert(!makeRecordIter(samReaderPaired, ENST00000373664, Minus).contains(DRR0237521778081_paired))

    // Read 1 plus strand
    assert(!makeRecordIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375225421084_paired))
    assert(makeRecordIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375225421084_paired))

    // Read 2 minus strand
    assert(!makeRecordIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375221918054_paired))
    assert(makeRecordIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375221918054_paired))

    // Read 2 plus strand
    assert(makeRecordIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375228168172_paired))
    assert(!makeRecordIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375228168172_paired))

    // Multiple isoforms
    assert(makeRecordIter(samReaderPaired, ENST00000373606, Minus).contains(DRR02375227278814_paired))
    assert(!makeRecordIter(samReaderPaired, ENST00000397152, Minus).contains(DRR02375227278814_paired))
    assert(!makeRecordIter(samReaderPaired, ENST00000373606, Minus).contains(DRR0237529822998_paired))
    assert(makeRecordIter(samReaderPaired, ENST00000397152, Minus).contains(DRR0237529822998_paired))

    // Constructed features for testing off by one errors
    assert(makeRecordIter(samReaderPaired, DRR0237527248016_contained1, Plus).contains(DRR0237527248016_paired))
    assert(makeRecordIter(samReaderPaired, DRR0237527248016_contained2, Plus).contains(DRR0237527248016_paired))
    assert(makeRecordIter(samReaderPaired, DRR0237527248016_contained3, Plus).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPaired, DRR0237527248016_notContained1, Plus).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPaired, DRR0237527248016_notContained2, Plus).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPaired, DRR0237527248016_notContained3, Plus).contains(DRR0237527248016_paired))
    assert(!makeRecordIter(samReaderPaired, DRR0237527248016_notContained4, Plus).contains(DRR0237527248016_paired))

  }

  test("Compatible records overlapping transcript - reads unpaired") {

    // DRR02375229686457_unpaired
    assert(makeRecordIter(samReaderUnpaired, ENST00000346199, Plus).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Plus).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000346199, Minus).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Minus).contains(DRR02375229686457_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000346199, Unstranded).contains(DRR02375229686457_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Unstranded).contains(DRR02375229686457_unpaired))

    // DRR0237526367658_unpaired
    assert(!makeRecordIter(samReaderUnpaired, ENST00000346199, Plus).contains(DRR0237526367658_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Plus).contains(DRR0237526367658_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000346199, Minus).contains(DRR0237526367658_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Minus).contains(DRR0237526367658_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000346199, Unstranded).contains(DRR0237526367658_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Unstranded).contains(DRR0237526367658_unpaired))

    // DRR02375217157817_unpaired
    assert(!makeRecordIter(samReaderUnpaired, ENST00000346199, Plus).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000445723, Plus).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000346199, Minus).contains(DRR02375217157817_unpaired))
    assert(!makeRecordIter(samReaderUnpaired, ENST00000445723, Minus).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000346199, Unstranded).contains(DRR02375217157817_unpaired))
    assert(makeRecordIter(samReaderUnpaired, ENST00000445723, Unstranded).contains(DRR02375217157817_unpaired))

  }

  test("Compatible records overlapping transcript - reads unpaired, short reference names") {

    // Block with illegal long reference name
    intercept[IllegalArgumentException]{makeRecordIter(samReaderUnpairedShortRefName,
      new GenericFeature(Block("chr20", 37682448, 37682602, Plus), None), Plus).size === 9}

    // DRR02375229686457_unpaired
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Plus).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Plus).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Minus).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Minus).contains(DRR02375229686457_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Unstranded).contains(DRR02375229686457_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Unstranded).contains(DRR02375229686457_unpaired_shortRefName))

    // DRR0237526367658_unpaired
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Plus).contains(DRR0237526367658_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Plus).contains(DRR0237526367658_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Minus).contains(DRR0237526367658_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Minus).contains(DRR0237526367658_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Unstranded).contains(DRR0237526367658_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Unstranded).contains(DRR0237526367658_unpaired_shortRefName))

    // DRR02375217157817_unpaired
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Plus).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Plus).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Minus).contains(DRR02375217157817_unpaired_shortRefName))
    assert(!makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Minus).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000346199, Unstranded).contains(DRR02375217157817_unpaired_shortRefName))
    assert(makeRecordIter(samReaderUnpairedShortRefName, ENST00000445723, Unstranded).contains(DRR02375217157817_unpaired_shortRefName))

  }

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

  def existsRecord(start: Int, end: Int, orientation: Orientation, name: String, fopStrand: Orientation): Boolean = {
    samReaderPaired.compatibleRecords(new GenericFeature(Block("20", start, end, orientation), None), fopStrand)
      .exists(rec => rec.getReadName == name)
  }

  def existsFragment(feat: Feature, name: String, fopStrand: Orientation): Boolean = {
    samReaderPaired.compatibleFragments(feat, fopStrand)
      .exists(pair => pair._1.getReadName == name && pair._2.getReadName == name
        && pair._1.getFirstOfPairFlag && !pair._2.getFirstOfPairFlag)  }

  def notExistsFragment(feat: Feature, name: String, fopStrand: Orientation): Boolean = {
    !samReaderPaired.compatibleFragments(feat, fopStrand)
      .exists(pair => pair._1.getReadName == name || pair._2.getReadName == name)  }

  test("Compatible fragments") {
    assert(samReaderPaired.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Plus).size === 4)
    // Unpaired
    intercept[IllegalArgumentException](samReaderUnpaired.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Plus))
    assert(samReaderPaired.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Minus).size === 1)
    assert(samReaderPaired.compatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Unstranded).size === 5)
    assert(existsFragment(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), "DRR023752.16260396", Plus))
    assert(existsFragment(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), "DRR023752.17105240", Minus))
    // No full pairs
    assert(samReaderPaired.compatibleFragments(ENST00000373508, Plus).size === 0)
    assert(samReaderPaired.compatibleFragments(ENST00000373508, Minus).size === 0)
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
    assert(samReaderUnpaired.countCompatibleFragments(ENST00000411780, Minus) === 2)
    assert(samReaderUnpaired.countCompatibleFragments(ENST00000411780, Unstranded) === 2)
    assert(samReaderUnpaired.countCompatibleFragments(ENST00000411780, Plus) === 0)
  }

  test("Count compatible fragments - reads paired") {
    assert(samReaderPaired.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Plus) === 4)
    assert(samReaderPaired.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Minus) === 1)
    assert(samReaderPaired.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Minus), None), Unstranded) === 5)
    assert(samReaderPaired.countCompatibleFragments(new GenericFeature(Block("20", 37500065, 37500486, Unstranded), None), Unstranded) === 5)
    // No full pairs
    assert(samReaderPaired.countCompatibleFragments(ENST00000373508, Plus) === 0)
    assert(samReaderPaired.countCompatibleFragments(ENST00000373508, Minus) === 0)
  }

}
