package testsequencing

import feature._
import htsjdk.samtools.SAMRecordIterator
import org.scalatest.FunSuite
import shared.GTF22Data._

import scala.collection.JavaConversions._

/**
  * Created by prussell on 1/27/17.
  */
class SamReaderUtilsSuite extends FunSuite {

  def makeIter(feat: Feature, firstOfPairStrandOnTranscript: Orientation): SAMRecordIterator =
    samReader.compatibleRecords(feat, firstOfPairStrandOnTranscript)

  test("Compatible records overlapping transcript") {

    // Read contains multiple blocks fully contained in exon
    val iter3 = makeIter(ENST00000373664, Plus)
    assert(!iter3.contains(DRR02375226601209))
    iter3.close()
    val iter11 = makeIter(ENST00000373664, Minus)
    assert(!iter11.contains(DRR02375226601209))
    iter11.close()

    // Mate unmapped
    val iter6 = makeIter(ENST00000525052, Plus)
    assert(iter6.contains(DRR0237521381132))
    iter6.close()
    val iter7 = makeIter(ENST00000525052, Minus)
    assert(!iter7.contains(DRR0237521381132))
    iter7.close()

    // Unstranded should add up to Plus + Minus
    val iter8 = makeIter(ENST00000525052, Plus)
    val s8 = iter8.size
    iter8.close()
    val iter9 = makeIter(ENST00000525052, Minus)
    val s9 = iter9.size
    iter9.close()
    val iter10 = makeIter(ENST00000525052, Unstranded)
    val s10 = iter10.size
    iter10.close()
    assert(s10 == s8 + s9)

    // Read 1 minus strand
    val iter1 = makeIter(ENST00000373664, Plus)
    assert(iter1.contains(DRR0237521778081))
    iter1.close()
    val iter2 = makeIter(ENST00000373664, Minus)
    assert(!iter2.contains(DRR0237521778081))
    iter2.close()

    // Read 1 plus strand
    val iter13 = makeIter(ENST00000373664, Plus)
    assert(!iter13.contains(DRR02375225421084))
    iter13.close()
    val iter12 = makeIter(ENST00000373664, Minus)
    assert(iter12.contains(DRR02375225421084))
    iter12.close()

    // Read 2 minus strand
    val iter14 = makeIter(ENST00000373664, Plus)
    assert(!iter14.contains(DRR02375221918054))
    iter14.close()
    val iter15 = makeIter(ENST00000373664, Minus)
    assert(iter15.contains(DRR02375221918054))
    iter15.close()

    // Read 2 plus strand
    val iter16 = makeIter(ENST00000373664, Plus)
    assert(iter16.contains(DRR02375228168172))
    iter16.close()
    val iter17 = makeIter(ENST00000373664, Minus)
    assert(!iter17.contains(DRR02375228168172))
    iter17.close()

    // Multiple isoforms
    val iter18 = makeIter(ENST00000373606, Minus)
    assert(iter18.contains(DRR02375227278814))
    iter18.close()
    val iter19 = makeIter(ENST00000397152, Minus)
    assert(!iter19.contains(DRR02375227278814))
    iter19.close()
    val iter20 = makeIter(ENST00000373606, Minus)
    assert(!iter20.contains(DRR0237529822998))
    iter20.close()
    val iter21 = makeIter(ENST00000397152, Minus)
    assert(iter21.contains(DRR0237529822998))
    iter21.close()

    // Constructed features for testing off by one errors
    val iter22 = makeIter(DRR0237527248016_contained1, Plus)
    assert(iter22.contains(DRR0237527248016))
    iter22.close()
    val iter23 = makeIter(DRR0237527248016_contained2, Plus)
    assert(iter23.contains(DRR0237527248016))
    iter23.close()
    val iter24 = makeIter(DRR0237527248016_contained3, Plus)
    assert(iter24.contains(DRR0237527248016))
    iter24.close()
    val iter25 = makeIter(DRR0237527248016_notContained1, Plus)
    assert(!iter25.contains(DRR0237527248016))
    iter25.close()
    val iter26 = makeIter(DRR0237527248016_notContained2, Plus)
    assert(!iter26.contains(DRR0237527248016))
    iter26.close()
    val iter27 = makeIter(DRR0237527248016_notContained3, Plus)
    assert(!iter27.contains(DRR0237527248016))
    iter27.close()
    val iter28 = makeIter(DRR0237527248016_notContained4, Plus)
    assert(!iter28.contains(DRR0237527248016))
    iter28.close()


  }

}
