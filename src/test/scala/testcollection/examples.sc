import java.io.File
import collection.{FeatureSet, GTF22FeatureSet}
import feature._

// Load a set of features from a GTF file
val featureSet: FeatureSet[Feature] =
  new GTF22FeatureSet(new File(getClass.getResource("/Homo_sapiens.GRCh38.86.chr_20_21_22.gtf").getPath))

// Get overlappers of an interval
val intervalOverlap: Iterator[Feature] = featureSet.overlappers("chr20", 37476591, 37645612, Plus) // ENST00000062104, ENST00000346199, ENST00000450588, ENST00000621390

// Get overlappers of a feature
val ENST00000613961: Feature = new Transcript(
  Block("20", 37499892, 37503975, Minus),
  Some("ENST00000613961"), Some("ENSG00000166619"))
val featureOverlap: Iterator[Feature] = featureSet.overlappers(ENST00000613961) // ENST00000613961, ENST00000467603
