package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureBuilderSuite extends FunSuite {

  test("Illegal constructions") {
    // Overlapping blocks
    val fb1 = new FeatureBuilder
    fb1.addBlock(chr1_1000_2000_plus_1)
    fb1.addBlock(chr1_1500_2500_plus)
    fb1.setFeatureId("Name")
    intercept[IllegalArgumentException](fb1.get())
    // CDS start but no end or the reverse
    val fb2 = new FeatureBuilder
    fb2.addBlock(chr1_1000_2000_plus_1)
    fb2.setFeatureId("Name")
    fb2.setGeneId("Gene")
    fb2.setCdsStart(1500)
    intercept[IllegalArgumentException](fb2.get())
    val fb3 = new FeatureBuilder
    fb3.addBlock(chr1_1000_2000_plus_1)
    fb3.setFeatureId("Name")
    fb3.setGeneId("Gene")
    fb3.setCdsEnd(1500)
    intercept[IllegalArgumentException](fb3.get())
    // Different block orientations
    val fb4 = new FeatureBuilder
    fb4.addBlock(chr1_1000_2000_plus_1)
    fb4.addBlock(chr1_3000_4000_minus)
    fb4.setFeatureId("Name")
    intercept[IllegalArgumentException](fb4.get())
    // No blocks
    val fb5 = new FeatureBuilder
    fb5.setFeatureId("Name")
    intercept[IllegalArgumentException](fb5.get())
    val fb6 = new FeatureBuilder
    fb6.setFeatureId("Name")
    fb6.setGeneId("Gene")
    intercept[IllegalArgumentException](fb6.get())
    // CDS start or end outside blocks
    val fb7 = new FeatureBuilder
    fb7.addBlock(chr1_1000_2000_plus_1)
    fb7.setFeatureId("Name")
    fb7.setGeneId("Gene")
    fb7.setCdsStart(1500)
    fb7.setCdsEnd(2001)
    intercept[IllegalArgumentException](fb7.get())
    val fb8 = new FeatureBuilder
    fb8.addBlock(chr1_1000_2000_plus_1)
    fb8.setFeatureId("Name")
    fb8.setGeneId("Gene")
    fb8.setCdsStart(700)
    fb8.setCdsEnd(1300)
    intercept[IllegalArgumentException](fb8.get())
    // CDS too short or length not divisible by 3
    val fb9 = new FeatureBuilder
    fb9.addBlock(chr1_1000_2000_plus_1)
    fb9.setFeatureId("Name")
    fb9.setGeneId("Gene")
    fb9.setCdsStart(1200)
    fb9.setCdsEnd(1300)
    intercept[IllegalArgumentException](fb9.get())
    val fb10 = new FeatureBuilder
    fb10.addBlock(chr1_1000_2000_plus_1)
    fb10.setFeatureId("Name")
    fb10.setGeneId("Gene")
    fb10.setCdsStart(1200)
    fb10.setCdsEnd(1203)
    intercept[IllegalArgumentException](fb10.get())
    // Blocks on different chromosomes
    val fb11 = new FeatureBuilder
    fb11.addBlock(chr1_1000_2000_plus_1)
    fb11.addBlock(chr2_3000_4000_plus)
    fb11.setFeatureId("Name")
    intercept[IllegalArgumentException](fb11.get())
  }

  test("Build single-block generic feature") {
    // Has name
    val fb1 = new FeatureBuilder
    fb1.addBlock(chr1_1000_2000_unstranded)
    fb1.setFeatureId("Name")
    assert(fb1.get() === new GenericFeature(chr1_1000_2000_unstranded, Some("Name")))
    // No name
    val fb2 = new FeatureBuilder
    fb2.addBlock(chr1_1500_2500_minus)
    assert(fb2.get() === new GenericFeature(chr1_1500_2500_minus, None))
  }

  test("Build multi-block generic feature") {
    // Has name
    val fb1 = new FeatureBuilder
    fb1.addBlock(chr1_1500_2500_minus)
    fb1.addBlock(chr1_5_6_minus)
    fb1.addBlock(chr1_3000_4000_minus)
    fb1.setFeatureId("Name")
    assert(fb1.get() === new GenericFeature(BlockSet(List(
      chr1_5_6_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), Some("Name")))
    // No name
    val fb2 = new FeatureBuilder
    fb2.addBlock(chr1_1000_2000_plus_1)
    fb2.addBlock(chr1_3000_4000_plus)
    fb2.addBlock(chr1_500_600_plus)
    assert(fb2.get() === new GenericFeature(BlockSet(List(
      chr1_500_600_plus, chr1_1000_2000_plus_1, chr1_3000_4000_plus
    )), None))
  }

  test("Build single-block transcript") {
    // Has name
    val fb1 = new FeatureBuilder
    fb1.addBlock(chr1_1000_2000_plus)
    fb1.setFeatureId("Name")
    fb1.setGeneId("Gene")
    assert(fb1.get() === new Transcript(chr1_1000_2000_plus, Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder
    fb2.addBlock(chr1_1500_2500_minus)
    fb2.setGeneId("Gene")
    assert(fb2.get() === new Transcript(chr1_1500_2500_minus, None, Some("Gene")))
  }

  test("Build multi-block transcript") {
    // Has name
    val fb1 = new FeatureBuilder
    fb1.setGeneId("Gene")
    fb1.addBlock(chr1_1500_2500_minus)
    fb1.addBlock(chr1_5_6_minus)
    fb1.addBlock(chr1_3000_4000_minus)
    fb1.setFeatureId("Name")
    assert(fb1.get() === new Transcript(BlockSet(List(
      chr1_5_6_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder
    fb2.addBlock(chr1_1000_2000_plus_1)
    fb2.setGeneId("Gene")
    fb2.addBlock(chr1_3000_4000_plus)
    fb2.addBlock(chr1_500_600_plus)
    assert(fb2.get() === new Transcript(BlockSet(List(
      chr1_500_600_plus, chr1_1000_2000_plus_1, chr1_3000_4000_plus
    )), None, Some("Gene")))
  }

  test("Build single-block mRNA") {
    // Has name
    val fb1 = new FeatureBuilder
    fb1.addBlock(chr1_1000_2000_plus)
    fb1.setCdsEnd(1012)
    fb1.setFeatureId("Name")
    fb1.setGeneId("Gene")
    fb1.setCdsStart(1000)
    assert(fb1.get() === MessengerRNA(chr1_1000_2000_plus, 1000, 1012, Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder
    fb2.setCdsStart(2000)
    fb2.addBlock(chr1_1500_2500_minus)
    fb2.setGeneId("Gene")
    fb2.setCdsEnd(2030)
    assert(fb2.get() === MessengerRNA(chr1_1500_2500_minus, 2000, 2030, None, Some("Gene")))
    // No name or gene name
    val fb3 = new FeatureBuilder
    fb3.setCdsStart(2000)
    fb3.addBlock(chr1_1500_2500_minus)
    fb3.setCdsEnd(2030)
    assert(fb3.get() === MessengerRNA(chr1_1500_2500_minus, 2000, 2030, None, None))
    // Transcript name but no gene name
    val fb4 = new FeatureBuilder
    fb4.setCdsStart(2000)
    fb4.addBlock(chr1_1500_2500_minus)
    fb4.setCdsEnd(2030)
    fb4.setFeatureId("Name")
    assert(fb4.get() === MessengerRNA(chr1_1500_2500_minus, 2000, 2030, Some("Name"), None))
  }

  test("Build multi-block mRNA") {
    // Has name
    val fb1 = new FeatureBuilder
    fb1.addBlock(chr1_1000_2000_plus)
    fb1.addBlock(chr1_3000_4000_plus)
    fb1.setCdsEnd(3030)
    fb1.addBlock(chr1_500_600_plus)
    fb1.setFeatureId("Name")
    fb1.setGeneId("Gene")
    fb1.setCdsStart(1970)
    assert(fb1.get() === MessengerRNA(BlockSet(List(
      chr1_500_600_plus, chr1_1000_2000_plus, chr1_3000_4000_plus
    )), 1970, 3030, Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder
    fb2.setCdsStart(6)
    fb2.addBlock(chr1_1500_2500_minus)
    fb2.setGeneId("Gene")
    fb2.addBlock(chr1_3000_4000_minus)
    fb2.addBlock(chr1_6_7_minus)
    fb2.setCdsEnd(1505)
    assert(fb2.get() === MessengerRNA(BlockSet(List(
      chr1_6_7_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), 6, 1505, None, Some("Gene")))
    // No name or gene name
    val fb3 = new FeatureBuilder
    fb3.addBlock(chr1_6_7_minus)
    fb3.setCdsStart(2000)
    fb3.addBlock(chr1_1500_2500_minus)
    fb3.setCdsEnd(3100)
    fb3.addBlock(chr1_3000_4000_minus)
    assert(fb3.get() === MessengerRNA(BlockSet(List(
      chr1_6_7_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), 2000, 3100, None, None))
    // Transcript name but no gene name
    val fb4 = new FeatureBuilder
    fb4.setCdsStart(3000)
    fb4.addBlock(chr1_1500_2500_minus)
    fb4.setCdsEnd(3300)
    fb4.addBlock(chr1_3000_4000_minus)
    fb4.addBlock(chr1_6_7_minus)
    fb4.setFeatureId("Name")
    assert(fb4.get() === MessengerRNA(BlockSet(List(
      chr1_6_7_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), 3000, 3300, Some("Name"), None))
  }



}
