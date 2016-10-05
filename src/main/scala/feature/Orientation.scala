package feature

/**
  * Created by prussell on 8/29/16.
  * A feature orientation
  */
sealed abstract class Orientation

object Plus extends Orientation {override def toString: String = "+"}
object Minus extends Orientation {override def toString: String = "-"}
object Both extends Orientation {override def toString: String = "both"}
object Unstranded extends Orientation {override def toString: String = "unstranded"}

object Orientation {

  /**
    * Consensus of two [[Orientation]]s
    * @param os Pair of [[Orientation]]s
    * @return Consensus [[Orientation]]
    */
  private def consensus(os: (Orientation, Orientation)): Orientation = {
    os match {
      case (os._1, os._1) => os._1
      case (Plus, Minus) => Unstranded
      case (Minus, Plus) => Unstranded
      case (Both, os._2) => os._2
      case (os._1, Both) => os._1
      case (Unstranded, os._2) => Unstranded
      case (os._1, Unstranded) => Unstranded
      case _ => throw new IllegalArgumentException("Not implemented: " + os.toString())
    }
  }

  /**
    * Consensus of two [[Orientation]]s
    * @param o1 [[Orientation]] 1
    * @param o2 [[Orientation]] 2
    * @return Consensus [[Orientation]]
    */
  def consensus(o1: Orientation, o2: Orientation): Orientation = consensus((o1, o2))

  /**
    * Tells whether [[Feature]]s with the two [[Orientation]]s are eligible to overlap
    * based on their orientations
    * @param os Pair of [[Orientation]]s
    * @return True iff the two [[Orientation]]s are compatible
    */
  private def isCompatible(os: (Orientation, Orientation)): Boolean = {
    os match {
      case (os._1, os._1) => true
      case (Plus, Minus) => false
      case (Minus, Plus) => false
      case (Unstranded, os._2) => false
      case (os._1, Unstranded) => false
      case (Both, os._2) => true
      case (os._1, Both) => true
      case _ => throw new IllegalArgumentException("Not implemented: " + os.toString())
    }
  }

  /**
    * Tells whether [[Feature]]s with the two [[Orientation]]s are eligible to overlap
    * based on their orientations
    * @param o1 [[Orientation]] 1
    * @param o2 [[Orientation]] 2
    * @return True iff the two [[Orientation]]s are compatible
    */
  def isCompatible(o1: Orientation, o2: Orientation): Boolean = isCompatible((o1, o2))

  /**
    * Tells whether the [[Feature]]s are eligible to overlap / have compatible orientations
    * @param f1 Feature 1
    * @param f2 Feature 2
    * @return True iff the two orientations are compatible
    */
  def isCompatible(f1: Feature, f2: Feature): Boolean = {
    if(f1 == Empty || f2 == Empty) false
    else isCompatible((f1.orientation, f2.orientation))
  }

}