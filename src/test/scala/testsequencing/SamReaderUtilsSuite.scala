package testsequencing

import feature._
import htsjdk.samtools.SAMRecord
import org.scalatest.FunSuite
import sequencing.SamReader
import shared.GTF22Data._

/**
  * Created by prussell on 1/27/17.
  */
class SamReaderUtilsSuite extends FunSuite {

  def makeIter(reader: SamReader, feat: Feature, firstOfPairStrandOnTranscript: Orientation): Iterator[SAMRecord] =
    reader.compatibleRecords(feat, firstOfPairStrandOnTranscript)

  test("Compatible records overlapping transcript - reads paired") {

    // Read contains multiple blocks fully contained in exon
    assert(!makeIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375226601209_paired))
    assert(!makeIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375226601209_paired))

    // Mate unmapped
    assert(makeIter(samReaderPaired, ENST00000525052, Plus).contains(DRR0237521381132_paired))
    assert(!makeIter(samReaderPaired, ENST00000525052, Minus).contains(DRR0237521381132_paired))

    // Unstranded should add up to Plus + Minus
    val s8 = makeIter(samReaderPaired, ENST00000525052, Plus).size
    val s9 = makeIter(samReaderPaired, ENST00000525052, Minus).size
    val s10 = makeIter(samReaderPaired, ENST00000525052, Unstranded).size
    assert(s10 == s8 + s9)

    // Read 1 minus strand
    assert(makeIter(samReaderPaired, ENST00000373664, Plus).contains(DRR0237521778081_paired))
    assert(!makeIter(samReaderPaired, ENST00000373664, Minus).contains(DRR0237521778081_paired))

    // Read 1 plus strand
    assert(!makeIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375225421084_paired))
    assert(makeIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375225421084_paired))

    // Read 2 minus strand
    assert(!makeIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375221918054_paired))
    assert(makeIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375221918054_paired))

    // Read 2 plus strand
    assert(makeIter(samReaderPaired, ENST00000373664, Plus).contains(DRR02375228168172_paired))
    assert(!makeIter(samReaderPaired, ENST00000373664, Minus).contains(DRR02375228168172_paired))

    // Multiple isoforms
    assert(makeIter(samReaderPaired, ENST00000373606, Minus).contains(DRR02375227278814_paired))
    assert(!makeIter(samReaderPaired, ENST00000397152, Minus).contains(DRR02375227278814_paired))
    assert(!makeIter(samReaderPaired, ENST00000373606, Minus).contains(DRR0237529822998_paired))
    assert(makeIter(samReaderPaired, ENST00000397152, Minus).contains(DRR0237529822998_paired))

    // Constructed features for testing off by one errors
    assert(makeIter(samReaderPaired, DRR0237527248016_contained1, Plus).contains(DRR0237527248016_paired))
    assert(makeIter(samReaderPaired, DRR0237527248016_contained2, Plus).contains(DRR0237527248016_paired))
    assert(makeIter(samReaderPaired, DRR0237527248016_contained3, Plus).contains(DRR0237527248016_paired))
    assert(!makeIter(samReaderPaired, DRR0237527248016_notContained1, Plus).contains(DRR0237527248016_paired))
    assert(!makeIter(samReaderPaired, DRR0237527248016_notContained2, Plus).contains(DRR0237527248016_paired))
    assert(!makeIter(samReaderPaired, DRR0237527248016_notContained3, Plus).contains(DRR0237527248016_paired))
    assert(!makeIter(samReaderPaired, DRR0237527248016_notContained4, Plus).contains(DRR0237527248016_paired))

  }

  test("Compatible records overlapping transcript - reads unpaired") {

    // DRR02375229686457_unpaired
    assert(makeIter(samReaderUnpaired, ENST00000346199, Plus).contains(DRR02375229686457_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Plus).contains(DRR02375229686457_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000346199, Minus).contains(DRR02375229686457_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Minus).contains(DRR02375229686457_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000346199, Unstranded).contains(DRR02375229686457_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Unstranded).contains(DRR02375229686457_unpaired))

    // DRR0237526367658_unpaired
    assert(!makeIter(samReaderUnpaired, ENST00000346199, Plus).contains(DRR0237526367658_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Plus).contains(DRR0237526367658_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000346199, Minus).contains(DRR0237526367658_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Minus).contains(DRR0237526367658_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000346199, Unstranded).contains(DRR0237526367658_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Unstranded).contains(DRR0237526367658_unpaired))

    // DRR02375217157817_unpaired
    assert(!makeIter(samReaderUnpaired, ENST00000346199, Plus).contains(DRR02375217157817_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000445723, Plus).contains(DRR02375217157817_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000346199, Minus).contains(DRR02375217157817_unpaired))
    assert(!makeIter(samReaderUnpaired, ENST00000445723, Minus).contains(DRR02375217157817_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000346199, Unstranded).contains(DRR02375217157817_unpaired))
    assert(makeIter(samReaderUnpaired, ENST00000445723, Unstranded).contains(DRR02375217157817_unpaired))

  }

}
