package reader

/**
  * Created by prussell on 12/15/16.
  */
object ReaderTest extends App {

  override def main(args: Array[String]): Unit = {

    val features = GTF2Reader.load("resources/Homo_sapiens.GRCh38.86.chr22.gtf")

    println(s"Number of features: ${features.size}")

    features.foreach(f => println(f.toString))

  }

}
