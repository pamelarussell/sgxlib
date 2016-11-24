package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class MessengerRNAMiscSuite extends FunSuite {

  test("Illegal CDS") {

    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 110, 115, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 199, 301, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 99, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 140, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 300, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 250, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 50, 60, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 450, 460, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 250, 260, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 299, 310, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 201, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 200, 350, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 200, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 150, 160, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_plus, 190, 310, Some("name"), Some("gene")) }

    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 110, 115, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 199, 301, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 99, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 140, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 300, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 250, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 401, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 50, 60, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 450, 460, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 150, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 250, 260, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 299, 310, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 201, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 200, 350, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 200, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 300, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 150, 160, Some("name"), Some("gene")) }
    intercept[IllegalArgumentException]{ MessengerRNA(chr1_100_200_300_400_minus, 190, 310, Some("name"), Some("gene")) }

  }

  test("getCDS") {
    // mRNA with one block
    // Strictly contained
    assert(MessengerRNA(chr1_1000_2000_minus, 1500, 1599, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1500, 1599, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1500, 1599, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1500, 1599, Plus))
    // CDS is whole mRNA
    assert(MessengerRNA(Block("chr1", 1000, 1999, Minus), 1000, 1999, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1000, 1999, Minus))
    assert(MessengerRNA(Block("chr1", 1000, 1999, Plus), 1000, 1999, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1000, 1999, Plus))
    // mRNA and CDS share one endpoint
    assert(MessengerRNA(chr1_1000_2000_minus, 1000, 1300, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1000, 1300, Minus))
    assert(MessengerRNA(chr1_1000_2000_minus, 1700, 2000, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1700, 2000, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1000, 1300, Plus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1700, 2000, Some("name"), Some("gene"))
      .getCDS === Block("chr1", 1700, 2000, Plus))
    // mRNA with two blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 110, 122, Some("name"), Some("gene")).getCDS
      === Block("chr1", 110, 122, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 310, 322, Some("name"), Some("gene")).getCDS
      === Block("chr1", 310, 322, Plus))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 150, 349, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 150, 200, Plus), Block("chr1", 300, 349, Plus))))
    // CDS equal to one exon
    assert(MessengerRNA(BlockSet(List(Block("chr1", 100, 199, Plus), Block("chr1", 300, 400, Plus))),
      100, 199, Some("name"), Some("gene")).getCDS
      === Block("chr1", 100, 199, Plus))
    assert(MessengerRNA(BlockSet(List(Block("chr1", 100, 200, Plus), Block("chr1", 300, 399, Plus))),
      300, 399, Some("name"), Some("gene")).getCDS
      === Block("chr1", 300, 399, Plus))
    // CDS equal to entire mRNA
    assert(MessengerRNA(BlockSet(List(Block("chr1", 100, 199, Plus), Block("chr1", 300, 399, Plus))),
      100, 399, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 100, 199, Plus), Block("chr1", 300, 399, Plus))))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 139, Some("name"), Some("gene")).getCDS
      === Block("chr1", 100, 139, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 370, 400, Some("name"), Some("gene")).getCDS
      === Block("chr1", 370, 400, Plus))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 180, 400, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 180, 200, Plus), Block("chr1", 300, 400, Plus))))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 350, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 100, 200, Plus), Block("chr1", 300, 350, Plus))))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 170, 200, Some("name"), Some("gene")).getCDS
      === Block("chr1", 170, 200, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 300, 330, Some("name"), Some("gene")).getCDS
      === Block("chr1", 300, 330, Plus))
    // mRNA with four blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 1599, Some("name"), Some("gene")).getCDS
      === Block("chr1", 1500, 1599, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 3599, Some("name"), Some("gene")).getCDS
      === Block("chr1", 3500, 3599, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 5599, Some("name"), Some("gene")).getCDS
      === Block("chr1", 5500, 5599, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7500, 7599, Some("name"), Some("gene")).getCDS
      === Block("chr1", 7500, 7599, Plus))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 3499, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 1500, 2000, Plus), Block("chr1", 3000, 3499, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 5499, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 3500, 4000, Plus), Block("chr1", 5000, 5499, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 7499, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 5500, 6000, Plus), Block("chr1", 7000, 7499, Plus))))
    // Strictly contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 5501, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 1500, 2000, Plus),
       Block("chr1", 3000, 4000, Plus),
       Block("chr1", 5000, 5501, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 7501, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("chr1", 3500, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 7501, Plus))))
    // Strictly contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 7500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1500, 2000, Plus),
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 7500, Plus))))
    // CDS equal to three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 6000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1000, 2000, Plus),
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 8000, Plus))))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 1600, Some("name"), Some("gene")).getCDS
      === Block("chr1", 1000, 1600, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7400, 8000, Some("name"), Some("gene")).getCDS
      === Block("chr1", 7400, 8000, Plus))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 3500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1000, 2000, Plus),
      Block("chr1", 3000, 3500, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 5500, 6000, Plus),
      Block("chr1", 7000, 8000, Plus))))
    // CDS sharing an endpoint with mRNA, contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 5400, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1000, 2000, Plus),
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 5400, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3600, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 3600, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 8000, Plus))))
    // CDS sharing an endpoint with mRNA, contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 7600, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1000, 2000, Plus),
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 7600, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1400, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1400, 2000, Plus),
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 8000, Plus))))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1400, 2000, Some("name"), Some("gene")).getCDS
      === Block("chr1", 1400, 2000, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 3600, Some("name"), Some("gene")).getCDS
      === Block("chr1", 3000, 3600, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5400, 6000, Some("name"), Some("gene")).getCDS
      === Block("chr1", 5400, 6000, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7000, 7600, Some("name"), Some("gene")).getCDS
      === Block("chr1", 7000, 7600, Plus))
    // CDS sharing an inner endpoint with one exon, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 4000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1500, 2000, Plus),
      Block("chr1", 3000, 4000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 6000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 3500, 4000, Plus),
      Block("chr1", 5000, 6000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 5500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 5500, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5000, 7500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 7500, Plus))))
    // CDS sharing an inner endpoint with one exon, contained three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1600, 6000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 1600, 2000, Plus),
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 7400, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("chr1", 3000, 4000, Plus),
      Block("chr1", 5000, 6000, Plus),
      Block("chr1", 7000, 7400, Plus))))
  }


}
