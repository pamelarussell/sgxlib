package testfeature

import feature._
import feature.FeatureBuilder
import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureBuilderSuite extends FunSuite {

  test("Illegal constructions") {
    // Overlapping blocks
    val fb1 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .addBlock(chr1_1500_2500_plus)
    .setFeatureId("Name")
    intercept[IllegalArgumentException](fb1.get())
    // CDS start but no end or the reverse
    val fb2 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(1500)
    intercept[IllegalArgumentException](fb2.get())
    val fb3 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsEnd(1500)
    intercept[IllegalArgumentException](fb3.get())
    // Different block orientations
    val fb4 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .addBlock(chr1_3000_4000_minus)
    .setFeatureId("Name")
    intercept[IllegalArgumentException](fb4.get())
    // No blocks
    val fb5 = new FeatureBuilder()
    .setFeatureId("Name")
    intercept[IllegalArgumentException](fb5.get())
    val fb6 = new FeatureBuilder()
    .setFeatureId("Name")
    .setGeneId("Gene")
    intercept[IllegalArgumentException](fb6.get())
    // CDS start or end outside blocks
    val fb7 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(1500)
    .setCdsEnd(2001)
    intercept[IllegalArgumentException](fb7.get())
    val fb8 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(700)
    .setCdsEnd(1300)
    intercept[IllegalArgumentException](fb8.get())
    // CDS too short or length not divisible by 3
    val fb9 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(1200)
    .setCdsEnd(1300)
    intercept[IllegalArgumentException](fb9.get())
    val fb10 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(1200)
    .setCdsEnd(1203)
    intercept[IllegalArgumentException](fb10.get())
    // Blocks on different chromosomes
    val fb11 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .addBlock(chr2_3000_4000_plus)
    .setFeatureId("Name")
    intercept[IllegalArgumentException](fb11.get())
  }

  test("Build single-block generic feature") {
    // Has name
    val fb1 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_unstranded)
    .setFeatureId("Name")
    assert(fb1.get() === new GenericFeature(chr1_1000_2000_unstranded, Some("Name")))
    // No name
    val fb2 = new FeatureBuilder()
    .addBlock(chr1_1500_2500_minus)
    assert(fb2.get() === new GenericFeature(chr1_1500_2500_minus, None))
  }

  test("Build multi-block generic feature") {
    // Has name
    val fb1 = new FeatureBuilder()
    .addBlock(chr1_1500_2500_minus)
    .addBlock(chr1_5_6_minus)
    .addBlock(chr1_3000_4000_minus)
    .setFeatureId("Name")
    assert(fb1.get() === new GenericFeature(BlockSet(List(
      chr1_5_6_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), Some("Name")))
    // No name
    val fb2 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .addBlock(chr1_3000_4000_plus)
    .addBlock(chr1_500_600_plus)
    assert(fb2.get() === new GenericFeature(BlockSet(List(
      chr1_500_600_plus, chr1_1000_2000_plus_1, chr1_3000_4000_plus
    )), None))
  }

  test("Build single-block transcript") {
    // Has name
    val fb1 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus)
    .setFeatureId("Name")
    .setGeneId("Gene")
    assert(fb1.get() === new Transcript(chr1_1000_2000_plus, Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder()
    .addBlock(chr1_1500_2500_minus)
    .setGeneId("Gene")
    assert(fb2.get() === new Transcript(chr1_1500_2500_minus, None, Some("Gene")))
  }

  test("Build multi-block transcript") {
    // Has name
    val fb1 = new FeatureBuilder()
    .setGeneId("Gene")
    .addBlock(chr1_1500_2500_minus)
    .addBlock(chr1_5_6_minus)
    .addBlock(chr1_3000_4000_minus)
    .setFeatureId("Name")
    assert(fb1.get() === new Transcript(BlockSet(List(
      chr1_5_6_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus_1)
    .setGeneId("Gene")
    .addBlock(chr1_3000_4000_plus)
    .addBlock(chr1_500_600_plus)
    assert(fb2.get() === new Transcript(BlockSet(List(
      chr1_500_600_plus, chr1_1000_2000_plus_1, chr1_3000_4000_plus
    )), None, Some("Gene")))
  }

  test("Build single-block mRNA") {
    // Has name
    val fb1 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus)
    .setCdsEnd(1012)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(1000)
    assert(fb1.get() === MessengerRNA(chr1_1000_2000_plus, 1000, 1012, Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder()
    .setCdsStart(2000)
    .addBlock(chr1_1500_2500_minus)
    .setGeneId("Gene")
    .setCdsEnd(2030)
    assert(fb2.get() === MessengerRNA(chr1_1500_2500_minus, 2000, 2030, None, Some("Gene")))
    // No name or gene name
    val fb3 = new FeatureBuilder()
    .setCdsStart(2000)
    .addBlock(chr1_1500_2500_minus)
    .setCdsEnd(2030)
    assert(fb3.get() === MessengerRNA(chr1_1500_2500_minus, 2000, 2030, None, None))
    // Transcript name but no gene name
    val fb4 = new FeatureBuilder()
    .setCdsStart(2000)
    .addBlock(chr1_1500_2500_minus)
    .setCdsEnd(2030)
    .setFeatureId("Name")
    assert(fb4.get() === MessengerRNA(chr1_1500_2500_minus, 2000, 2030, Some("Name"), None))
  }

  test("Build multi-block mRNA") {
    // Has name
    val fb1 = new FeatureBuilder()
    .addBlock(chr1_1000_2000_plus)
    .addBlock(chr1_3000_4000_plus)
    .setCdsEnd(3030)
    .addBlock(chr1_500_600_plus)
    .setFeatureId("Name")
    .setGeneId("Gene")
    .setCdsStart(1970)
    assert(fb1.get() === MessengerRNA(BlockSet(List(
      chr1_500_600_plus, chr1_1000_2000_plus, chr1_3000_4000_plus
    )), 1970, 3030, Some("Name"), Some("Gene")))
    // No name
    val fb2 = new FeatureBuilder()
    .setCdsStart(6)
    .addBlock(chr1_1500_2500_minus)
    .setGeneId("Gene")
    .addBlock(chr1_3000_4000_minus)
    .addBlock(chr1_6_7_minus)
    .setCdsEnd(1505)
    assert(fb2.get() === MessengerRNA(BlockSet(List(
      chr1_6_7_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), 6, 1505, None, Some("Gene")))
    // No name or gene name
    val fb3 = new FeatureBuilder()
    .addBlock(chr1_6_7_minus)
    .setCdsStart(2000)
    .addBlock(chr1_1500_2500_minus)
    .setCdsEnd(3100)
    .addBlock(chr1_3000_4000_minus)
    assert(fb3.get() === MessengerRNA(BlockSet(List(
      chr1_6_7_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), 2000, 3100, None, None))
    // Transcript name but no gene name
    val fb4 = new FeatureBuilder()
    .setCdsStart(3000)
    .addBlock(chr1_1500_2500_minus)
    .setCdsEnd(3300)
    .addBlock(chr1_3000_4000_minus)
    .addBlock(chr1_6_7_minus)
    .setFeatureId("Name")
    assert(fb4.get() === MessengerRNA(BlockSet(List(
      chr1_6_7_minus, chr1_1500_2500_minus, chr1_3000_4000_minus
    )), 3000, 3300, Some("Name"), None))
  }



}
