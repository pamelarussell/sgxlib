package testformat

import feature._
import org.scalatest.FunSuite
import feature.format.{Exon, GTF22Record, Ignore}

/**
  * Created by prussell on 12/18/16.
  */
class GTF2RecordSuite extends FunSuite {

  val line1 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";\n\n\n"
  val line2 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";#comment"
  val line3 = "1\thavana\texon\t12179\t12227\t500\t+\t2\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";#comment\n"
  val line4 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\"; #comment"
  val selenocysteine = "22\tensembl_havana\tSelenocysteine\t50217358\t50217360\t.\t+\t.\tgene_id \"ENSG00000073169\"; gene_version \"13\"; transcript_id \"ENST00000380903\"; transcript_version \"6\"; gene_name \"SELO\"; gene_source \"ensembl_havana\"; gene_biotype \"protein_coding\"; havana_gene \"OTTHUMG00000044645\"; havana_gene_version \"3\"; transcript_name \"SELO-001\"; transcript_source \"ensembl_havana\"; transcript_biotype \"protein_coding\"; tag \"CCDS\"; ccds_id \"CCDS43034\"; havana_transcript \"OTTHUMT00000075003\"; havana_transcript_version \"2\"; tag \"seleno\"; tag \"basic\"; transcript_support_level \"1\";"
  val intergenic = "22\tensembl_havana\tinter\t50217358\t50217360\t.\t+\t.\tgene_id \"\"; gene_version \"\"; transcript_id \"\"; transcript_version \"\"; gene_name \"\"; gene_source \"ensembl_havana\"; gene_biotype \"\"; havana_gene \"\"; havana_gene_version \"\"; transcript_name \"\"; transcript_source \"ensembl_havana\"; transcript_biotype \"inter\"; tag \"CCDS\"; ccds_id \"CCDS43034\"; havana_transcript \"\"; havana_transcript_version \"\"; tag \"inter\"; tag \"basic\"; transcript_support_level \"1\";"

  val invalidOrientation = "1\thavana\texon\t12179\t12227\t.\t.\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";\n\n\n"
  val missingField1 = "1\thavana\texon\t12179\t12227\t\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val missingField2 = "1\thavana\texon\t12179\t12227\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val noGeneId = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val noTranscriptId = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidStartStop1 = "1\thavana\texon\t12179\t12178\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidStartStop2 = "1\thavana\texon\t0\t12179\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidStartStop3 = "1\thavana\texon\tx\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidStartStop4 = "1\thavana\texon\t12179\tx\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidFrame1 = "1\thavana\texon\t12179\t12227\t.\t+\t3\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidFrame2 = "1\thavana\texon\t12179\t12227\t.\t+\tx\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidScore = "1\thavana\texon\t12179\t12227\tx\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidSpacing1 = "1 havana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidSpacing2 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version  \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidSpacing3 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\";  transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidSpacing4 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\";\ttranscript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val invalidAttribute1 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\" gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField1 = "x\t1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField2 = "1\tx\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField3 = "1\thavana\tx\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField4 = "1\thavana\texon\tx\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField5 = "1\thavana\texon\t12179\tx\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField6 = "1\thavana\texon\t12179\t12227\tx\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField7 = "1\thavana\texon\t12179\t12227\t.\tx\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField8 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tx\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField9 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; x\tgene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";"
  val extraField10 = "1\thavana\texon\t12179\t12227\t.\t+\t.\tgene_id \"ENSG00000223972\"; gene_version \"5\"; transcript_id \"ENST00000450305\"; transcript_version \"2\"; exon_number \"2\"; gene_name \"DDX11L1\"; gene_source \"havana\"; gene_biotype \"transcribed_unprocessed_pseudogene\"; havana_gene \"OTTHUMG00000000961\"; havana_gene_version \"2\"; transcript_name \"DDX11L1-001\"; transcript_source \"havana\"; transcript_biotype \"transcribed_unprocessed_pseudogene\"; havana_transcript \"OTTHUMT00000002844\"; havana_transcript_version \"2\"; exon_id \"ENSE00001671638\"; exon_version \"2\"; tag \"basic\"; transcript_support_level \"NA\";\tx"

  test("Ignored feature type") {
    val gr = new GTF22Record(selenocysteine)
    assert(gr.featureType === Ignore)
    val fb = new FeatureBuilder()
    intercept[IllegalArgumentException](gr.op(fb))
  }

  test("Illegal feature builder operations") {

    // Mismatched gene and transcript IDs
    val fbNoTranscript = new FeatureBuilder(geneId = Some("ENSG00000223972"))
    val fbNoGene = new FeatureBuilder(featureId = Some("ENST00000450305"))
    val fbMismatchedGeneTranscript1 = new FeatureBuilder(geneId = Some("ENSG00000223972"), featureId = Some("fakeTranscript"))
    val fbMismatchedGeneTranscript2 = new FeatureBuilder(geneId = Some("fakeGene"), featureId = Some("ENST00000450305"))
    val gr1 = new GTF22Record(line1)
    intercept[IllegalArgumentException](gr1.op(fbNoTranscript))
    intercept[IllegalArgumentException](gr1.op(fbNoGene))
    intercept[IllegalArgumentException](gr1.op(fbMismatchedGeneTranscript1))
    intercept[IllegalArgumentException](gr1.op(fbMismatchedGeneTranscript2))

    // Feature type issues
    val fbInter = new FeatureBuilder(geneId = None, featureId = None, blocks = List(Block("22", 1000, 2000, Plus)))
    val gr2 = new GTF22Record(intergenic)
    intercept[IllegalArgumentException](gr2.op(fbInter))
  }

  test("Invalid lines") {
    intercept[IllegalArgumentException](new GTF22Record(invalidOrientation))
    intercept[IllegalArgumentException](new GTF22Record(missingField1))
    intercept[IllegalArgumentException](new GTF22Record(missingField2))
    intercept[IllegalArgumentException](new GTF22Record(noGeneId))
    intercept[IllegalArgumentException](new GTF22Record(noTranscriptId))
    intercept[IllegalArgumentException](new GTF22Record(invalidStartStop1))
    intercept[IllegalArgumentException](new GTF22Record(invalidStartStop2))
    intercept[IllegalArgumentException](new GTF22Record(invalidStartStop3))
    intercept[IllegalArgumentException](new GTF22Record(invalidStartStop4))
    intercept[IllegalArgumentException](new GTF22Record(invalidFrame1))
    intercept[IllegalArgumentException](new GTF22Record(invalidFrame2))
    intercept[IllegalArgumentException](new GTF22Record(invalidScore))
    intercept[IllegalArgumentException](new GTF22Record(invalidSpacing1))
    intercept[IllegalArgumentException](new GTF22Record(invalidSpacing2))
    intercept[IllegalArgumentException](new GTF22Record(invalidSpacing3))
    intercept[IllegalArgumentException](new GTF22Record(invalidSpacing4))
    intercept[IllegalArgumentException](new GTF22Record(invalidAttribute1))
    intercept[IllegalArgumentException](new GTF22Record(extraField1))
    intercept[IllegalArgumentException](new GTF22Record(extraField2))
    intercept[IllegalArgumentException](new GTF22Record(extraField3))
    intercept[IllegalArgumentException](new GTF22Record(extraField4))
    intercept[IllegalArgumentException](new GTF22Record(extraField5))
    intercept[IllegalArgumentException](new GTF22Record(extraField6))
    intercept[IllegalArgumentException](new GTF22Record(extraField7))
    intercept[IllegalArgumentException](new GTF22Record(extraField8))
    intercept[IllegalArgumentException](new GTF22Record(extraField9))
    intercept[IllegalArgumentException](new GTF22Record(extraField10))
  }

  def checkFieldValues(line: String): Unit = {
    val rec = new GTF22Record(line)
    assert(rec.chr === "1")
    assert(rec.source === "havana")
    assert(rec.featureType === Exon)
    assert(rec.start === 12178)
    assert(rec.end === 12227)
    assert(rec.score === None)
    assert(rec.orientation === Plus)
    assert(rec.frame === None)
    assert(rec.attributes.toSet.diff(Map("gene_id" -> "ENSG00000223972",
      "gene_version" -> "5",
      "transcript_id" -> "ENST00000450305",
      "transcript_version" -> "2",
      "exon_number" -> "2",
      "gene_name" -> "DDX11L1",
      "gene_source" -> "havana",
      "gene_biotype" -> "transcribed_unprocessed_pseudogene",
      "havana_gene" -> "OTTHUMG00000000961",
      "havana_gene_version" -> "2",
      "transcript_name" -> "DDX11L1-001",
      "transcript_source" -> "havana",
      "transcript_biotype" -> "transcribed_unprocessed_pseudogene",
      "havana_transcript" -> "OTTHUMT00000002844",
      "havana_transcript_version" -> "2",
      "exon_id" -> "ENSE00001671638",
      "exon_version" -> "2",
      "tag" -> "basic",
      "transcript_support_level" -> "NA").toSet) === Set.empty)
  }

  test("Field values") {
    checkFieldValues(line1)
    checkFieldValues(line2)
    checkFieldValues(line4)
  }

  test("Score and frame") {
    val rec = new GTF22Record(line3)
    assert(rec.score === Some(500))
    assert(rec.frame === Some(2))
  }

}
