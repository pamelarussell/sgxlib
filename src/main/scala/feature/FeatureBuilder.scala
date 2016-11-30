package feature

final class FeatureBuilder {

  private val blocks: scala.collection.mutable.TreeSet[Region] = new scala.collection.mutable.TreeSet[Region]()
  private var cdsStart: Option[Int] = None
  private var cdsEnd: Option[Int] = None
  private var transcriptId: Option[String] = None
  private var geneId: Option[String] = None
  private var featureName: Option[String] = None

  def addBlock(b: Block): Unit = blocks.add(b)
  def setCdsStart(s: Int): Unit = cdsStart = Some(s)
  def setCdsEnd(e: Int): Unit = cdsEnd = Some(e)
  def setTranscriptId(t: String): Unit = transcriptId = Some(t)
  def setGeneId(g: String): Unit = geneId = Some(g)
  def setFeatureName(f: String): Unit = featureName = Some(f)

  private final case class Data(blocks: List[Block],
                                cdsStart: Option[Int],
                                cdsEnd: Option[Int],
                                transcriptId: Option[String],
                                geneId: Option[String],
                                featureName: Option[String])

  def get(): Feature = {

    // Convert mutable treeset of regions to immutable list of blocks
    def regionsToBlocks(bs: scala.collection.mutable.TreeSet[Region]): List[Block] = {
      blocks.toList.map {
        case b: Block => b
        case _ => throw new IllegalArgumentException("Not a Block")
      }
    }

    val data = Data(regionsToBlocks(blocks), cdsStart, cdsEnd, transcriptId, geneId, featureName)

    data match {
      // mRNA with one block
      case Data(blk :: Nil, Some(cs), Some(ce), t, g, None) => MessengerRNA(blk, cs, ce, t, g)
      // mRNA with multiple blocks
      case Data(blk1 :: blk2 :: tail, Some(cs), Some(ce), t, g, None) => MessengerRNA(BlockSet(data.blocks), cs, ce, t, g)
      // ncRNA with one block
      case Data(blk :: Nil, None, None, t, g, None) => new Transcript(blk, t, g)
      // ncRNA with multiple blocks
      case Data(blk1 :: blk2 :: tail, None, None, t, g, None) => new Transcript(BlockSet(data.blocks), t, g)
      // Generic block
      case Data(blk :: Nil, None, None, None, None, f) => new GenericFeature(blk, f)
      // Generic block set
      case Data(blk1 :: blk2 :: tail, None, None, None, None, f) => new GenericFeature(BlockSet(data.blocks), f)
      // None of the above
      case _ => throw new IllegalArgumentException(s"Invalid feature builder:\n" +
        s"Blocks:\t${data.blocks.toString()}\n" +
        s"CDS start:\t${data.cdsStart.toString}\n" +
        s"CDS end:\t${data.cdsEnd.toString}\n" +
        s"Transcript ID:\t${data.transcriptId.toString}\n" +
        s"Gene ID:\t${data.geneId.toString}\n" +
        s"Feature name:\t${data.featureName.toString}\n")
    }

  }

}
