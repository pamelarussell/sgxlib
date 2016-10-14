package feature

/**
  * Created by prussell on 10/14/16.
  */
class Feature(region: Region, name: String) {



}

trait Transcript extends Feature

trait Coding extends Transcript {
  val cdsStart: Int
  val cdsEnd: Int
}

