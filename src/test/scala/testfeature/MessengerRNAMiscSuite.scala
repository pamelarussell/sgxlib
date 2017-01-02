package testfeature

import feature._
import feature.{MessengerRNA, Minus, Plus}
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class MessengerRNAMiscSuite extends FunSuite {

  test("Illegal orientation") {
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_both, 1000, 1012, Some("name"), Some("gene")))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_unstranded, 1000, 1012, Some("name"), Some("gene")))
  }

  test("Illegal names") {
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_plus_1, 1000, 1012, Some(""), Some("gene")))
    intercept[IllegalArgumentException](MessengerRNA(chr1_1000_2000_plus_1, 1000, 1012, Some("name"), Some("")))
  }

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
      .getCDS === Block("1", 1500, 1599, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1500, 1599, Some("name"), Some("gene"))
      .getCDS === Block("1", 1500, 1599, Plus))
    // CDS is whole mRNA
    assert(MessengerRNA(Block("1", 1000, 1999, Minus), 1000, 1999, Some("name"), Some("gene"))
      .getCDS === Block("1", 1000, 1999, Minus))
    assert(MessengerRNA(Block("1", 1000, 1999, Plus), 1000, 1999, Some("name"), Some("gene"))
      .getCDS === Block("1", 1000, 1999, Plus))
    // mRNA and CDS share one endpoint
    assert(MessengerRNA(chr1_1000_2000_minus, 1000, 1300, Some("name"), Some("gene"))
      .getCDS === Block("1", 1000, 1300, Minus))
    assert(MessengerRNA(chr1_1000_2000_minus, 1700, 2000, Some("name"), Some("gene"))
      .getCDS === Block("1", 1700, 2000, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))
      .getCDS === Block("1", 1000, 1300, Plus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1700, 2000, Some("name"), Some("gene"))
      .getCDS === Block("1", 1700, 2000, Plus))
    // mRNA with two blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 110, 122, Some("name"), Some("gene")).getCDS
      === Block("1", 110, 122, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_minus, 310, 322, Some("name"), Some("gene")).getCDS
      === Block("1", 310, 322, Minus))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 150, 349, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 150, 200, Plus), Block("1", 300, 349, Plus))))
    // CDS equal to one exon
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 400, Plus))),
      100, 199, Some("name"), Some("gene")).getCDS
      === Block("1", 100, 199, Plus))
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 200, Plus), Block("1", 300, 399, Plus))),
      300, 399, Some("name"), Some("gene")).getCDS
      === Block("1", 300, 399, Plus))
    // CDS equal to entire mRNA
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 399, Plus))),
      100, 399, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 399, Plus))))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_minus, 100, 139, Some("name"), Some("gene")).getCDS
      === Block("1", 100, 139, Minus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 370, 400, Some("name"), Some("gene")).getCDS
      === Block("1", 370, 400, Plus))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 180, 400, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 180, 200, Plus), Block("1", 300, 400, Plus))))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 350, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 100, 200, Plus), Block("1", 300, 350, Plus))))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 170, 200, Some("name"), Some("gene")).getCDS
      === Block("1", 170, 200, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 300, 330, Some("name"), Some("gene")).getCDS
      === Block("1", 300, 330, Plus))
    // mRNA with four blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1500, 1599, Some("name"), Some("gene")).getCDS
      === Block("1", 1500, 1599, Minus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 3599, Some("name"), Some("gene")).getCDS
      === Block("1", 3500, 3599, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5500, 5599, Some("name"), Some("gene")).getCDS
      === Block("1", 5500, 5599, Minus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7500, 7599, Some("name"), Some("gene")).getCDS
      === Block("1", 7500, 7599, Plus))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 3499, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 1500, 2000, Plus), Block("1", 3000, 3499, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 5499, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 3500, 4000, Minus), Block("1", 5000, 5499, Minus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 7499, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 5500, 6000, Plus), Block("1", 7000, 7499, Plus))))
    // Strictly contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 5501, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 1500, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 5501, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 7501, Some("name"), Some("gene")).getCDS
      === BlockSet(List(Block("1", 3500, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 7501, Minus))))
    // Strictly contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 7500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1500, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 7500, Plus))))
    // CDS equal to three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 6000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus))))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1600, Some("name"), Some("gene")).getCDS
      === Block("1", 1000, 1600, Minus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7400, 8000, Some("name"), Some("gene")).getCDS
      === Block("1", 7400, 8000, Plus))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 3500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 3500, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5500, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 5500, 6000, Minus),
      Block("1", 7000, 8000, Minus))))
    // CDS sharing an endpoint with mRNA, contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 5400, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 5400, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3600, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 3600, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus))))
    // CDS sharing an endpoint with mRNA, contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 7600, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 7600, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1400, 8000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1400, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus))))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1400, 2000, Some("name"), Some("gene")).getCDS
      === Block("1", 1400, 2000, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 3600, Some("name"), Some("gene")).getCDS
      === Block("1", 3000, 3600, Minus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5400, 6000, Some("name"), Some("gene")).getCDS
      === Block("1", 5400, 6000, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 7000, 7600, Some("name"), Some("gene")).getCDS
      === Block("1", 7000, 7600, Minus))
    // CDS sharing an inner endpoint with one exon, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 4000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1500, 2000, Plus),
      Block("1", 3000, 4000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 6000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 3500, 4000, Minus),
      Block("1", 5000, 6000, Minus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 5500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 5500, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5000, 7500, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 7500, Minus))))
    // CDS sharing an inner endpoint with one exon, contained three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1600, 6000, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 1600, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 7400, Some("name"), Some("gene")).getCDS
      === BlockSet(List(
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 7400, Plus))))
  }

  test("get 3'-UTR") {
    // mRNA with one block
    // Strictly contained
    assert(MessengerRNA(chr1_1000_2000_minus, 1500, 1599, Some("name"), Some("gene"))
      .get3UTR === Some(Block("1", 1000, 1500, Minus)))
    assert(MessengerRNA(chr1_1000_2000_plus, 1500, 1599, Some("name"), Some("gene"))
      .get3UTR === Some(Block("1", 1599, 2000, Plus)))
    // CDS is whole mRNA
    assert(MessengerRNA(Block("1", 1000, 1999, Minus), 1000, 1999, Some("name"), Some("gene"))
      .get3UTR === None)
    assert(MessengerRNA(Block("1", 1000, 1999, Plus), 1000, 1999, Some("name"), Some("gene"))
      .get3UTR === None)
    // mRNA and CDS share one endpoint
    assert(MessengerRNA(chr1_1000_2000_minus, 1000, 1300, Some("name"), Some("gene"))
      .get3UTR === None)
    assert(MessengerRNA(chr1_1000_2000_minus, 1700, 2000, Some("name"), Some("gene"))
      .get3UTR === Some(Block("1", 1000, 1700, Minus)))
    assert(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))
      .get3UTR === Some(Block("1", 1300, 2000, Plus)))
    assert(MessengerRNA(chr1_1000_2000_plus, 1700, 2000, Some("name"), Some("gene"))
      .get3UTR === None)
    // mRNA with two blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 110, 122, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 122, 200, Plus),
      Block("1", 300, 400, Plus)))))
    assert(MessengerRNA(chr1_100_200_300_400_minus, 310, 322, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 100, 200, Minus),
      Block("1", 300, 310, Minus)))))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 150, 349, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 349, 400, Plus)))
    // CDS equal to one exon
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 400, Plus))),
      100, 199, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 300, 400, Plus)))
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 200, Plus), Block("1", 300, 399, Plus))),
      300, 399, Some("name"), Some("gene")).get3UTR
      === None)
    // CDS equal to entire mRNA
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 399, Plus))),
      100, 399, Some("name"), Some("gene")).get3UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_minus, 100, 139, Some("name"), Some("gene")).get3UTR
      === None)
    assert(MessengerRNA(chr1_100_200_300_400_plus, 370, 400, Some("name"), Some("gene")).get3UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 180, 400, Some("name"), Some("gene")).get3UTR
      === None)
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 350, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 350, 400, Plus)))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 170, 200, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 300, 400, Plus)))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 300, 330, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 330, 400, Plus)))
    // mRNA with four blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1500, 1599, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 1000, 1500, Minus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 3599, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 3599, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5500, 5599, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 5500, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7500, 7599, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7599, 8000, Plus)))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 3499, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 3499, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 5499, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 3500, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 7499, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7499, 8000, Plus)))
    // Strictly contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 5501, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 5501, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 7501, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 3500, Minus)))))
    // Strictly contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 7500, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7500, 8000, Plus)))
    // CDS equal to three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 6000, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7000, 8000, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 8000, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 1000, 2000, Minus)))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1600, Some("name"), Some("gene")).get3UTR
      === None)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7400, 8000, Some("name"), Some("gene")).get3UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 3500, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 3500, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5500, 8000, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 5500, Minus)))))
    // CDS sharing an endpoint with mRNA, contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 5400, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 5400, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3600, 8000, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 3600, Minus)))))
    // CDS sharing an endpoint with mRNA, contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 7600, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7600, 8000, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1400, 8000, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 1000, 1400, Minus)))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1400, 2000, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 3600, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 1000, 2000, Minus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5400, 6000, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7000, 8000, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 7000, 7600, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 6000, Minus)))))
    // CDS sharing an inner endpoint with one exon, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 4000, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 6000, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 3500, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 5500, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 5500, 6000, Plus),
      Block("1", 7000, 8000, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5000, 7500, Some("name"), Some("gene")).get3UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Minus),
      Block("1", 3000, 4000, Minus)))))
    // CDS sharing an inner endpoint with one exon, contained three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1600, 6000, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7000, 8000, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 7400, Some("name"), Some("gene")).get3UTR
      === Some(Block("1", 7400, 8000, Plus)))
  }


  test("get 5’-UTR") {
    // mRNA with one block
    // Strictly contained
    assert(MessengerRNA(chr1_1000_2000_minus, 1500, 1599, Some("name"), Some("gene"))
      .get5UTR === Some(Block("1", 1599, 2000, Minus)))
    assert(MessengerRNA(chr1_1000_2000_plus, 1500, 1599, Some("name"), Some("gene"))
      .get5UTR === Some(Block("1", 1000, 1500, Plus)))
    // CDS is whole mRNA
    assert(MessengerRNA(Block("1", 1000, 1999, Minus), 1000, 1999, Some("name"), Some("gene"))
      .get5UTR === None)
    assert(MessengerRNA(Block("1", 1000, 1999, Plus), 1000, 1999, Some("name"), Some("gene"))
      .get5UTR === None)
    // mRNA and CDS share one endpoint
    assert(MessengerRNA(chr1_1000_2000_minus, 1000, 1300, Some("name"), Some("gene"))
      .get5UTR === Some(Block("1", 1300, 2000, Minus)))
    assert(MessengerRNA(chr1_1000_2000_minus, 1700, 2000, Some("name"), Some("gene"))
      .get5UTR === None)
    assert(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))
      .get5UTR === None)
    assert(MessengerRNA(chr1_1000_2000_plus, 1700, 2000, Some("name"), Some("gene"))
      .get5UTR === Some(Block("1", 1000, 1700, Plus)))
    // mRNA with two blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 110, 122, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 100, 110, Plus)))
    assert(MessengerRNA(chr1_100_200_300_400_minus, 310, 322, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 322, 400, Minus)))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 150, 349, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 100, 150, Plus)))
    // CDS equal to one exon
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 400, Plus))),
      100, 199, Some("name"), Some("gene")).get5UTR
      === None)
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 200, Plus), Block("1", 300, 399, Plus))),
      300, 399, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 100, 200, Plus)))
    // CDS equal to entire mRNA
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 399, Plus))),
      100, 399, Some("name"), Some("gene")).get5UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_minus, 100, 139, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 139, 200, Minus),
      Block("1", 300, 400, Minus)))))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 370, 400, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 100, 200, Plus),
      Block("1", 300, 370, Plus)))))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 180, 400, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 100, 180, Plus)))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 350, Some("name"), Some("gene")).get5UTR
      === None)
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 170, 200, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 100, 170, Plus)))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 300, 330, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 100, 200, Plus)))
    // mRNA with four blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1500, 1599, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1599, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 3599, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 3500, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5500, 5599, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 5599, 6000, Minus),
      Block("1", 7000, 8000, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7500, 7599, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 7500, Plus)))))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 3499, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 1500, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 5499, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 5499, 6000, Minus),
      Block("1", 7000, 8000, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 7499, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 5500, Plus)))))
    // Strictly contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 5501, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 1500, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 7501, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 7501, 8000, Minus)))
    // Strictly contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 7500, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 1500, Plus)))
    // CDS equal to three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 6000, Some("name"), Some("gene")).get5UTR
      === None)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 8000, Some("name"), Some("gene")).get5UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1600, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1600, 2000, Minus),
      Block("1", 3000, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 7400, 8000, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 6000, Plus),
      Block("1", 7000, 7400, Plus)))))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 3500, Some("name"), Some("gene")).get5UTR
      === None)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5500, 8000, Some("name"), Some("gene")).get5UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 5400, Some("name"), Some("gene")).get5UTR
      === None)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3600, 8000, Some("name"), Some("gene")).get5UTR
      === None)
    // CDS sharing an endpoint with mRNA, contained in four exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 7600, Some("name"), Some("gene")).get5UTR
      === None)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1400, 8000, Some("name"), Some("gene")).get5UTR
      === None)
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1400, 2000, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 1400, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 3600, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 3600, 4000, Minus),
      Block("1", 5000, 6000, Minus),
      Block("1", 7000, 8000, Minus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5400, 6000, Some("name"), Some("gene")).get5UTR
      === Some(BlockSet(List(
      Block("1", 1000, 2000, Plus),
      Block("1", 3000, 4000, Plus),
      Block("1", 5000, 5400, Plus)))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 7000, 7600, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 7600, 8000, Minus)))
    // CDS sharing an inner endpoint with one exon, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 4000, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 1500, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 6000, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 7000, 8000, Minus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 5500, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 2000, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5000, 7500, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 7500, 8000, Minus)))
    // CDS sharing an inner endpoint with one exon, contained three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1600, 6000, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 1600, Plus)))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 7400, Some("name"), Some("gene")).get5UTR
      === Some(Block("1", 1000, 2000, Plus)))
  }

  test("get start codon") {
    // mRNA with one block
    // Strictly contained
    assert(MessengerRNA(chr1_1000_2000_minus, 1500, 1599, Some("name"), Some("gene"))
    .getStartCodon === Block("1", 1596, 1599, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1500, 1599, Some("name"), Some("gene"))
    .getStartCodon === Block("1", 1500, 1503, Plus))
    // CDS is whole mRNA
    assert(MessengerRNA(Block("1", 1000, 1999, Minus), 1000, 1999, Some("name"), Some("gene"))
    .getStartCodon === Block("1", 1996, 1999, Minus))
    assert(MessengerRNA(Block("1", 1000, 1999, Plus), 1000, 1999, Some("name"), Some("gene"))
    .getStartCodon === Block("1", 1000, 1003, Plus))
    // mRNA with two blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 110, 122, Some("name"), Some("gene")).getStartCodon
    === Block("1", 110, 113, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_minus, 310, 322, Some("name"), Some("gene")).getStartCodon
    === Block("1", 319, 322, Minus))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 198, 349, Some("name"), Some("gene")).getStartCodon
    === BlockSet(List(
      Block("1", 198, 200, Plus),
      Block("1", 300, 301, Plus))))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_minus, 100, 199, Some("name"), Some("gene")).getStartCodon
    === Block("1", 196, 199, Minus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 370, 400, Some("name"), Some("gene")).getStartCodon
    === Block("1", 370, 373, Plus))
    // mRNA with four blocks
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1500, 3499, Some("name"), Some("gene")).getStartCodon
    === Block("1", 1500, 1503, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3980, 5001, Some("name"), Some("gene")).getStartCodon
    === BlockSet(List(
      Block("1", 3998, 4000, Minus),
      Block("1", 5000, 5001, Minus))))
    // Strictly contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1998, 5198, Some("name"), Some("gene")).getStartCodon
    === BlockSet(List(
      Block("1", 1998, 2000, Plus),
      Block("1", 3000, 3001, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3500, 7501, Some("name"), Some("gene")).getStartCodon
    === Block("1", 7498, 7501, Minus))
    // CDS equal to three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 6000, Some("name"), Some("gene")).getStartCodon
    === Block("1", 1000, 1003, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 8000, Some("name"), Some("gene")).getStartCodon
    === Block("1", 7997, 8000, Minus))
    // CDS sharing an endpoint with mRNA, contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1600, Some("name"), Some("gene")).getStartCodon
    === Block("1", 1597, 1600, Minus))
    // CDS sharing an inner endpoint with one exon, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1998, 4000, Some("name"), Some("gene")).getStartCodon
    === BlockSet(List(
      Block("1", 1998, 2000, Plus),
      Block("1", 3000, 3001, Plus))))
    // Start codon spread over 3 exons
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Plus),
        Block("1", 3000, 3001, Plus),
        Block("1", 4000, 5000, Plus)
    )), 1999, 4010, Some("name"), Some("gene")).getStartCodon === BlockSet(List(
      Block("1", 1999, 2000, Plus),
      Block("1", 3000, 3001, Plus),
      Block("1", 4000, 4001, Plus))))
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Minus),
        Block("1", 3000, 3001, Minus),
        Block("1", 4000, 5000, Minus)
    )), 1990, 4001, Some("name"), Some("gene")).getStartCodon === BlockSet(List(
      Block("1", 1999, 2000, Minus),
      Block("1", 3000, 3001, Minus),
      Block("1", 4000, 4001, Minus))))
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Plus),
        Block("1", 3000, 3001, Plus),
        Block("1", 4000, 4001, Plus),
        Block("1", 5000, 5001, Plus),
        Block("1", 6000, 7000, Plus)
      )), 3000, 6300, Some("name"), Some("gene")).getStartCodon === BlockSet(List(
      Block("1", 3000, 3001, Plus),
      Block("1", 4000, 4001, Plus),
      Block("1", 5000, 5001, Plus))))
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Minus),
        Block("1", 3000, 3001, Minus),
        Block("1", 4000, 4001, Minus),
        Block("1", 5000, 5001, Minus),
        Block("1", 6000, 7000, Minus)
      )), 1700, 5001, Some("name"), Some("gene")).getStartCodon === BlockSet(List(
      Block("1", 3000, 3001, Minus),
      Block("1", 4000, 4001, Minus),
      Block("1", 5000, 5001, Minus))))
  }

  test("get stop codon") {
    // mRNA with one block
    // Strictly contained
    assert(MessengerRNA(chr1_1000_2000_minus, 1500, 1599, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1500, 1503, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1500, 1599, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1596, 1599, Plus))
    // CDS is whole mRNA
    assert(MessengerRNA(Block("1", 1000, 1999, Minus), 1000, 1999, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1000, 1003, Minus))
    assert(MessengerRNA(Block("1", 1000, 1999, Plus), 1000, 1999, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1996, 1999, Plus))
    // mRNA and CDS share one endpoint
    assert(MessengerRNA(chr1_1000_2000_minus, 1000, 1300, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1000, 1003, Minus))
    assert(MessengerRNA(chr1_1000_2000_minus, 1700, 2000, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1700, 1703, Minus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1000, 1300, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1297, 1300, Plus))
    assert(MessengerRNA(chr1_1000_2000_plus, 1700, 2000, Some("name"), Some("gene"))
      .getStopCodon === Block("1", 1997, 2000, Plus))
    // mRNA with two blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 110, 122, Some("name"), Some("gene")).getStopCodon
      === Block("1", 119, 122, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_minus, 310, 322, Some("name"), Some("gene")).getStopCodon
      === Block("1", 310, 313, Minus))
    // CDS equal to entire mRNA
    assert(MessengerRNA(BlockSet(List(Block("1", 100, 199, Plus), Block("1", 300, 399, Plus))),
      100, 399, Some("name"), Some("gene")).getStopCodon
      === Block("1", 396, 399, Plus))
    // CDS sharing an endpoint with mRNA, contained in two exons
    assert(MessengerRNA(chr1_100_200_300_400_plus, 180, 400, Some("name"), Some("gene")).getStopCodon
      === Block("1", 397, 400, Plus))
    assert(MessengerRNA(chr1_100_200_300_400_plus, 100, 302, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 199, 200, Plus),
      Block("1", 300, 302, Plus))))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_100_200_300_400_plus, 170, 200, Some("name"), Some("gene")).getStopCodon
      === Block("1", 197, 200, Plus))
    // mRNA with four blocks
    // Strictly contained in one exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1500, 1599, Some("name"), Some("gene")).getStopCodon
      === Block("1", 1500, 1503, Minus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3500, 3599, Some("name"), Some("gene")).getStopCodon
      === Block("1", 3596, 3599, Plus))
    // Strictly contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1800, 3001, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 1998, 2000, Plus),
      Block("1", 3000, 3001, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3999, 5200, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 3999, 4000, Minus),
      Block("1", 5000, 5002, Minus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 5500, 7499, Some("name"), Some("gene")).getStopCodon
      === Block("1", 7496, 7499, Plus))
    // CDS equal to three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 6000, Some("name"), Some("gene")).getStopCodon
      === Block("1", 5997, 6000, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 8000, Some("name"), Some("gene")).getStopCodon
      === Block("1", 3000, 3003, Minus))
    // CDS sharing an endpoint with mRNA, contained in three exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1000, 5001, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 3998, 4000, Plus),
      Block("1", 5000, 5001, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3999, 8000, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 3999, 4000, Minus),
      Block("1", 5000, 5002, Minus))))
    // CDS sharing an inner endpoint with one exon, contained in exon
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 1400, 2000, Some("name"), Some("gene")).getStopCodon
      === Block("1", 1997, 2000, Plus))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3000, 3600, Some("name"), Some("gene")).getStopCodon
      === Block("1", 3000, 3003, Minus))
    // CDS sharing an inner endpoint with one exon, contained in two exons
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 3998, 6000, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 3998, 4000, Minus),
      Block("1", 5000, 5001, Minus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_plus, 3000, 5002, Some("name"), Some("gene")).getStopCodon
      === BlockSet(List(
      Block("1", 3999, 4000, Plus),
      Block("1", 5000, 5002, Plus))))
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 5000, 7500, Some("name"), Some("gene")).getStopCodon
      === Block("1", 5000, 5003, Minus))
    // Stop codon spread over 3 exons
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Minus),
        Block("1", 3000, 3001, Minus),
        Block("1", 4000, 5000, Minus)
      )), 1999, 4010, Some("name"), Some("gene")).getStopCodon === BlockSet(List(
      Block("1", 1999, 2000, Minus),
      Block("1", 3000, 3001, Minus),
      Block("1", 4000, 4001, Minus))))
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Plus),
        Block("1", 3000, 3001, Plus),
        Block("1", 4000, 5000, Plus)
      )), 1990, 4001, Some("name"), Some("gene")).getStopCodon === BlockSet(List(
      Block("1", 1999, 2000, Plus),
      Block("1", 3000, 3001, Plus),
      Block("1", 4000, 4001, Plus))))
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Minus),
        Block("1", 3000, 3001, Minus),
        Block("1", 4000, 4001, Minus),
        Block("1", 5000, 5001, Minus),
        Block("1", 6000, 7000, Minus)
      )), 3000, 6300, Some("name"), Some("gene")).getStopCodon === BlockSet(List(
      Block("1", 3000, 3001, Minus),
      Block("1", 4000, 4001, Minus),
      Block("1", 5000, 5001, Minus))))
    assert(MessengerRNA(BlockSet(
      List(Block("1", 1000, 2000, Plus),
        Block("1", 3000, 3001, Plus),
        Block("1", 4000, 4001, Plus),
        Block("1", 5000, 5001, Plus),
        Block("1", 6000, 7000, Plus)
      )), 1700, 5001, Some("name"), Some("gene")).getStopCodon === BlockSet(List(
      Block("1", 3000, 3001, Plus),
      Block("1", 4000, 4001, Plus),
      Block("1", 5000, 5001, Plus))))
  }


}
