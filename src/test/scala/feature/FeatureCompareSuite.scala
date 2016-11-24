package feature

import org.scalatest.FunSuite

/**
  * Created by prussell on 11/12/16.
  */
class FeatureCompareSuite extends FunSuite {

  test("GenericFeature compare to GenericFeature") {
    // Same blocks, same name
    assert(new GenericFeature(chr1_1000_2000_minus, None).compare(new GenericFeature(chr1_1000_2000_minus, None)) === 0)
    // Same blocks, different name
    assert(new GenericFeature(chr1_1000_2000_minus, None).compare(new GenericFeature(chr1_1000_2000_minus, Some("xxxx"))) > 0)
    assert(new GenericFeature(chr1_1000_2000_minus, Some("xxxx")).compare(new GenericFeature(chr1_1000_2000_minus, None)) < 0)
    // Different blocks, same name
    assert(new GenericFeature(chr1_1000_2000_minus, None).compare(new GenericFeature(chr1_1000_2000_both, None)) < 0)
    assert(new GenericFeature(chr1_1000_2000_both, None).compare(new GenericFeature(chr1_1000_2000_minus, None)) > 0)
    // Different blocks, different name
    assert(new GenericFeature(chr1_1000_2000_minus, None).compare(new GenericFeature(chr1_1000_2000_both, Some("name"))) < 0)
    assert(new GenericFeature(chr1_1000_2000_both, None).compare(new GenericFeature(chr1_1000_2000_minus, Some("name"))) > 0)
  }

  test("GenericFeature compare to Transcript") {
    // Same blocks, same name
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name")).compare(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene"))) < 0)
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).compare(new GenericFeature(chr1_900_1100_1200_1300_plus, Some("name"))) > 0)
    // Same blocks, different name
    assert(new GenericFeature(chr1_900_1100_1200_1300_plus, None).compare(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene"))) > 0)
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).compare(new GenericFeature(chr1_900_1100_1200_1300_plus, None)) < 0)
    // Different blocks, same name
    assert(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name")).compare(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene"))) > 0)
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("name"), Some("gene")).compare(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name"))) < 0)
    // Different blocks, different name
    assert(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name")).compare(new Transcript(chr1_900_1100_1200_1300_plus, Some("xxxx"), Some("gene"))) > 0)
    assert(new Transcript(chr1_900_1100_1200_1300_plus, Some("xxxx"), Some("gene")).compare(new GenericFeature(chr1_1100_1200_1300_1400_plus, Some("name"))) < 0)
  }

  test("GenericFeature compare to mRNA") {
    // Same blocks, same name
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, Some("name"))
      .compare(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("name"), Some("gene"))
      .compare(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, Some("name"))) > 0)
     // Same blocks, different name
    assert(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, None)
      .compare(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("name"), Some("gene"))
      .compare(new GenericFeature(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, None)) < 0)
    // Different blocks, same name
    assert(new GenericFeature(chr1_7000_8000_9000_10000_minus, Some("name"))
      .compare(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("name"), Some("gene"))
      .compare(new GenericFeature(chr1_7000_8000_9000_10000_minus, Some("name"))) < 0)
    // Different blocks, different name
    assert(new GenericFeature(chr1_7000_8000_9000_10000_minus, Some("name"))
      .compare(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("xxxx"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_1000_2000_3000_4000_5000_6000_7000_8000_minus, 1000, 1099, Some("xxxx"), Some("gene"))
      .compare(new GenericFeature(chr1_7000_8000_9000_10000_minus, Some("name"))) < 0)
  }

  test("Transcript compare to Transcript") {
    // Same blocks, same name, same gene
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene"))) === 0)
    // Same blocks, different name, same gene
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, None, Some("gene"))) < 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, None, Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene"))) > 0)
    // Different blocks, same name, same gene
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene"))) > 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    // Different blocks, different name, same gene
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("xxxx"), Some("gene"))) > 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("xxxx"), Some("gene")).compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    // Same blocks, same name, different gene
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene"))) > 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx"))) < 0)
    // Same blocks, different name, different gene
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("xxxx"), Some("gene"))) < 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("xxxx"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, same name, different gene
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx"))) > 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx")).compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    // Different blocks, different name, different gene
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene")).compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("xxxx"), Some("xxxx"))) > 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("xxxx"), Some("xxxx")).compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
  }

  test("Transcript compare to mRNA") {
    // Same blocks, same name, same gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))
      .compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene"))) > 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))) < 0)
    // Same blocks, different name, same gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))
      .compare(new Transcript(chr1_6000_8000_9000_10000_plus, None, Some("gene"))) < 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, None, Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))) > 0)
    // Different blocks, same name, same gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))
      .compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))) > 0)
    // Different blocks, different name, same gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("xxxx"), Some("gene"))
      .compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("xxxx"), Some("gene"))) > 0)
    // Same blocks, same name, different gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))
      .compare(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx"))) < 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("gene"))) > 0)
    // Same blocks, different name, different gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("xxxx"))
      .compare(new Transcript(chr1_6000_8000_9000_10000_plus, None, Some("gene"))) < 0)
    assert(new Transcript(chr1_6000_8000_9000_10000_plus, None, Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, same name, different gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("xxxx"))
      .compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, different name, different gene
    assert(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("xxxx"), Some("xxxx"))
      .compare(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))) < 0)
    assert(new Transcript(chr1_7000_8000_9000_10000_plus, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_6000_8000_9000_10000_plus, 7000, 7099, Some("xxxx"), Some("xxxx"))) > 0)
  }

  test("mRNA compare to mRNA") {
    // Same blocks, same name, same gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) === 0)
    // Same blocks, different name, same gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, None, Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))) < 0)
    // Different blocks, same name, same gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    // Different blocks, different name, same gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("xxxx"), Some("gene"))) > 0)
    // Same blocks, same name, different gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), None)) < 0)
    // Same blocks, different name, different gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, same name, different gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, different name, different gene, same CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("xxxx"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("xxxx"), Some("xxxx"))) > 0)
    // Same blocks, same name, same gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    // Same blocks, different name, same gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("xxxx"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    // Different blocks, same name, same gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    // Different blocks, different name, same gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("xxxx"), Some("gene"))) > 0)
    // Same blocks, same name, different gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("xxxx"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    // Same blocks, different name, different gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, None, Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("xxxx"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, None, Some("gene"))) < 0)
    // Different blocks, same name, different gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, different name, different gene, different CDS start, same CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, None, Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 900, 999, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1099, None, Some("xxxx"))) > 0)
    // Same blocks, same name, same gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, Some("name"), Some("gene"))) > 0)
    // Same blocks, different name, same gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, None, Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, None, Some("gene"))) < 0)
    // Different blocks, same name, same gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1220, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1220, Some("name"), Some("gene"))) > 0)
    // Different blocks, different name, same gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1220, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1220, Some("xxxx"), Some("gene"))) > 0)
    // Same blocks, same name, different gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, Some("name"), Some("xxxx"))) < 0)
    // Same blocks, different name, different gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, None, Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, None, Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1048, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, same name, different gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1048, Some("name"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1048, Some("name"), Some("xxxx"))) > 0)
    // Different blocks, different name, different gene, same CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1048, Some("xxxx"), Some("xxxx"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1000, 1048, Some("xxxx"), Some("xxxx"))) > 0)
    // Same blocks, same name, same gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("name"), Some("gene"))) < 0)
    // Same blocks, different name, same gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("name"), Some("gene"))) > 0)
    // Different blocks, same name, same gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, Some("name"), Some("gene"))) > 0)
    // Different blocks, different name, same gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, None, Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, None, Some("gene"))) > 0)
    // Same blocks, same name, different gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), None)) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), None)
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("name"), Some("gene"))) > 0)
    // Same blocks, different name, different gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("xxxx"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), None)) > 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), None)
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1020, 1050, Some("xxxx"), Some("gene"))) < 0)
    // Different blocks, same name, different gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, Some("name"), None)
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, Some("name"), None)) > 0)
    // Different blocks, different name, different gene, different CDS start, different CDS end
    assert(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, None, None)
      .compare(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))) < 0)
    assert(MessengerRNA(chr1_900_1100_1900_2100_2900_4100_plus, 1000, 1099, Some("name"), Some("gene"))
      .compare(MessengerRNA(chr1_900_1100_1200_1300_plus, 1050, 1240, None, None)) > 0)
  }

}
