
/** Provides classes to represent genomic features, i.e., annotations with block coordinates, an orientation,
  * and other associated information.
  *
  * The main classes to use are [[feature.GenericFeature]], [[feature.Transcript]], and [[feature.MessengerRNA]].
  * These can be instantiated directly or by using a [[feature.FeatureBuilder]].
  *
  * In addition, genomic annotations with no associated information can be represented with [[feature.Block]],
  * a single contiguous block, and [[feature.BlockSet]], an annotation consisting of multiple blocks.
  */
package object feature
