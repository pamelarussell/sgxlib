import htsjdk.samtools.SAMRecord

object Hello {

  val record = new SAMRecord(null)

  def main(args: Array[String]) = {
    println("Hello!")
    println(record.toString)
  }
}
