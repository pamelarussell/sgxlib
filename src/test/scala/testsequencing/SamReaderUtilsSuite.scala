package testsequencing

import feature._
import htsjdk.samtools.SAMRecordIterator
import org.scalatest.FunSuite
import sequencing.SamReader
import shared.GTF22Data._

import scala.collection.JavaConversions._

/**
  * Created by prussell on 1/27/17.
  */
class SamReaderUtilsSuite extends FunSuite {

  def makeIter(reader: SamReader, feat: Feature, firstOfPairStrandOnTranscript: Orientation): SAMRecordIterator =
    reader.compatibleRecords(feat, firstOfPairStrandOnTranscript)

  test("Compatible records overlapping transcript - reads paired") {

    // Read contains multiple blocks fully contained in exon
    val iter3 = makeIter(samReaderPaired, ENST00000373664, Plus)
    assert(!iter3.contains(DRR02375226601209_paired))
    iter3.close()
    val iter11 = makeIter(samReaderPaired, ENST00000373664, Minus)
    assert(!iter11.contains(DRR02375226601209_paired))
    iter11.close()

    // Mate unmapped
    val iter6 = makeIter(samReaderPaired, ENST00000525052, Plus)
    assert(iter6.contains(DRR0237521381132_paired))
    iter6.close()
    val iter7 = makeIter(samReaderPaired, ENST00000525052, Minus)
    assert(!iter7.contains(DRR0237521381132_paired))
    iter7.close()

    // Unstranded should add up to Plus + Minus
    val iter8 = makeIter(samReaderPaired, ENST00000525052, Plus)
    val s8 = iter8.size
    iter8.close()
    val iter9 = makeIter(samReaderPaired, ENST00000525052, Minus)
    val s9 = iter9.size
    iter9.close()
    val iter10 = makeIter(samReaderPaired, ENST00000525052, Unstranded)
    val s10 = iter10.size
    iter10.close()
    assert(s10 == s8 + s9)

    // Read 1 minus strand
    val iter1 = makeIter(samReaderPaired, ENST00000373664, Plus)
    assert(iter1.contains(DRR0237521778081_paired))
    iter1.close()
    val iter2 = makeIter(samReaderPaired, ENST00000373664, Minus)
    assert(!iter2.contains(DRR0237521778081_paired))
    iter2.close()

    // Read 1 plus strand
    val iter13 = makeIter(samReaderPaired, ENST00000373664, Plus)
    assert(!iter13.contains(DRR02375225421084_paired))
    iter13.close()
    val iter12 = makeIter(samReaderPaired, ENST00000373664, Minus)
    assert(iter12.contains(DRR02375225421084_paired))
    iter12.close()

    // Read 2 minus strand
    val iter14 = makeIter(samReaderPaired, ENST00000373664, Plus)
    assert(!iter14.contains(DRR02375221918054_paired))
    iter14.close()
    val iter15 = makeIter(samReaderPaired, ENST00000373664, Minus)
    assert(iter15.contains(DRR02375221918054_paired))
    iter15.close()

    // Read 2 plus strand
    val iter16 = makeIter(samReaderPaired, ENST00000373664, Plus)
    assert(iter16.contains(DRR02375228168172_paired))
    iter16.close()
    val iter17 = makeIter(samReaderPaired, ENST00000373664, Minus)
    assert(!iter17.contains(DRR02375228168172_paired))
    iter17.close()

    // Multiple isoforms
    val iter18 = makeIter(samReaderPaired, ENST00000373606, Minus)
    assert(iter18.contains(DRR02375227278814_paired))
    iter18.close()
    val iter19 = makeIter(samReaderPaired, ENST00000397152, Minus)
    assert(!iter19.contains(DRR02375227278814_paired))
    iter19.close()
    val iter20 = makeIter(samReaderPaired, ENST00000373606, Minus)
    assert(!iter20.contains(DRR0237529822998_paired))
    iter20.close()
    val iter21 = makeIter(samReaderPaired, ENST00000397152, Minus)
    assert(iter21.contains(DRR0237529822998_paired))
    iter21.close()

    // Constructed features for testing off by one errors
    val iter22 = makeIter(samReaderPaired, DRR0237527248016_contained1, Plus)
    assert(iter22.contains(DRR0237527248016_paired))
    iter22.close()
    val iter23 = makeIter(samReaderPaired, DRR0237527248016_contained2, Plus)
    assert(iter23.contains(DRR0237527248016_paired))
    iter23.close()
    val iter24 = makeIter(samReaderPaired, DRR0237527248016_contained3, Plus)
    assert(iter24.contains(DRR0237527248016_paired))
    iter24.close()
    val iter25 = makeIter(samReaderPaired, DRR0237527248016_notContained1, Plus)
    assert(!iter25.contains(DRR0237527248016_paired))
    iter25.close()
    val iter26 = makeIter(samReaderPaired, DRR0237527248016_notContained2, Plus)
    assert(!iter26.contains(DRR0237527248016_paired))
    iter26.close()
    val iter27 = makeIter(samReaderPaired, DRR0237527248016_notContained3, Plus)
    assert(!iter27.contains(DRR0237527248016_paired))
    iter27.close()
    val iter28 = makeIter(samReaderPaired, DRR0237527248016_notContained4, Plus)
    assert(!iter28.contains(DRR0237527248016_paired))
    iter28.close()

  }

  test("Compatible records overlapping transcript - reads unpaired") {

    // DRR02375229686457_unpaired
    val iter1 = makeIter(samReaderUnpaired, ENST00000346199, Plus)
    assert(iter1.contains(DRR02375229686457_unpaired))
    iter1.close()
    val iter2 = makeIter(samReaderUnpaired, ENST00000445723, Plus)
    assert(!iter2.contains(DRR02375229686457_unpaired))
    iter2.close()
    val iter3 = makeIter(samReaderUnpaired, ENST00000346199, Minus)
    assert(!iter3.contains(DRR02375229686457_unpaired))
    iter3.close()
    val iter4 = makeIter(samReaderUnpaired, ENST00000445723, Minus)
    assert(!iter4.contains(DRR02375229686457_unpaired))
    iter4.close()
    val iter5 = makeIter(samReaderUnpaired, ENST00000346199, Unstranded)
    assert(iter5.contains(DRR02375229686457_unpaired))
    iter5.close()
    val iter6 = makeIter(samReaderUnpaired, ENST00000445723, Unstranded)
    assert(!iter6.contains(DRR02375229686457_unpaired))
    iter6.close()

    // DRR0237526367658_unpaired
    val iter7 = makeIter(samReaderUnpaired, ENST00000346199, Plus)
    assert(!iter7.contains(DRR0237526367658_unpaired))
    iter7.close()
    val iter8 = makeIter(samReaderUnpaired, ENST00000445723, Plus)
    assert(!iter8.contains(DRR0237526367658_unpaired))
    iter8.close()
    val iter9 = makeIter(samReaderUnpaired, ENST00000346199, Minus)
    assert(iter9.contains(DRR0237526367658_unpaired))
    iter9.close()
    val iter10 = makeIter(samReaderUnpaired, ENST00000445723, Minus)
    assert(!iter10.contains(DRR0237526367658_unpaired))
    iter10.close()
    val iter11 = makeIter(samReaderUnpaired, ENST00000346199, Unstranded)
    assert(iter11.contains(DRR0237526367658_unpaired))
    iter11.close()
    val iter12 = makeIter(samReaderUnpaired, ENST00000445723, Unstranded)
    assert(!iter12.contains(DRR0237526367658_unpaired))
    iter12.close()

    // DRR02375217157817_unpaired
    val iter13 = makeIter(samReaderUnpaired, ENST00000346199, Plus)
    assert(!iter13.contains(DRR02375217157817_unpaired))
    iter13.close()
    val iter14 = makeIter(samReaderUnpaired, ENST00000445723, Plus)
    assert(iter14.contains(DRR02375217157817_unpaired))
    iter14.close()
    val iter15 = makeIter(samReaderUnpaired, ENST00000346199, Minus)
    assert(iter15.contains(DRR02375217157817_unpaired))
    iter15.close()
    val iter16 = makeIter(samReaderUnpaired, ENST00000445723, Minus)
    assert(!iter16.contains(DRR02375217157817_unpaired))
    iter16.close()
    val iter17 = makeIter(samReaderUnpaired, ENST00000346199, Unstranded)
    assert(iter17.contains(DRR02375217157817_unpaired))
    iter17.close()
    val iter18 = makeIter(samReaderUnpaired, ENST00000445723, Unstranded)
    assert(iter18.contains(DRR02375217157817_unpaired))
    iter18.close()

  }

}
