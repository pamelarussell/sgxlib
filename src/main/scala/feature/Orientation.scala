package feature

/** An orientation for a genomic feature.
  *
  * Can refer to a particular DNA strand if the feature is associated with
  * a strand, or alternatives if not.
  */
sealed trait Orientation

/** The plus strand of DNA */
object Plus extends Orientation {override def toString: String = "+"}
/** The minus strand of DNA */
object Minus extends Orientation {override def toString: String = "-"}
/** Both strands of DNA */
object Both extends Orientation {override def toString: String = "both"}
/** Orientation for features not associated with a DNA strand */
object Unstranded extends Orientation {override def toString: String = "unstranded"}

/** Utility methods for calculations on [[Orientation]]s */
object Orientation {

  /** Returns the reverse of the specified [[Orientation]].
    *
    * [[Plus]] and [[Minus]] are swapped by this function. [[Both]] and [[Unstranded]] are unchanged.
    *
    * @param o [[Orientation]] to get the reverse of
    * @return The reverse of the specified [[Orientation]]
    */
  def invert(o: Orientation): Orientation = {
    o match {
      case Plus => Minus
      case Minus => Plus
      case Both => Both
      case Unstranded => Unstranded
    }
  }

  private def consensus(os: (Orientation, Orientation)): Orientation = {
    os match {
      case (os._1, os._1) => os._1
      case (Plus, Minus) => Unstranded
      case (Minus, Plus) => Unstranded
      case (Both, os._2) => os._2
      case (os._1, Both) => os._1
      case (Unstranded, os._2) => Unstranded
      case (os._1, Unstranded) => Unstranded; case _ => throw new UnsupportedOperationException("Not implemented: " + os.toString())
    }
  }

  /** Returns the consensus of two possibly different [[Orientation]]s.
    *
    * The consensus is [[Unstranded]] if the two [[Orientation]]s are incompatible.
    *
    * @param o1 [[Orientation]] 1
    * @param o2 [[Orientation]] 2
    * @return The consensus [[Orientation]]
    */
  def consensus(o1: Orientation, o2: Orientation): Orientation = consensus((o1, o2))

  private def isCompatible(os: (Orientation, Orientation)): Boolean = {
    os match {
      case (os._1, os._1) => true
      case (Plus, Minus) => false
      case (Minus, Plus) => false
      case (Unstranded, os._2) => false
      case (os._1, Unstranded) => false
      case (Both, os._2) => true
      case (os._1, Both) => true; case _ => throw new UnsupportedOperationException("Not implemented: " + os.toString())
    }
  }

  /** Returns a boolean value representing whether two [[Orientation]]s are compatible.
    *
    * The notion of compatibility is used when, e.g., merging or intersecting [[Feature]]s.
    *
    * Every [[Orientation]] is compatible with itself.
    * [[Plus]] and [[Minus]] are incompatible with each other.
    * [[Unstranded]] is incompatible with every other orientation.
    * [[Both]] is compatible with [[Plus]] and [[Minus]].
    *
    * @param o1 [[Orientation]] 1
    * @param o2 [[Orientation]] 2
    * @return The boolean value indicating compatibility
    */
  def isCompatible(o1: Orientation, o2: Orientation): Boolean = isCompatible((o1, o2))

  /** Returns a boolean value representing whether the [[Orientation]]s of two [[Region]]s are compatible.
    *
    * The notion of compatibility is used when, e.g., merging or intersecting [[Region]]s.
    *
    * Every [[Orientation]] is compatible with itself.
    * [[Plus]] and [[Minus]] are incompatible with each other.
    * [[Unstranded]] is incompatible with every other orientation.
    * [[Both]] is compatible with [[Plus]] and [[Minus]].
    *
    * @param f1 [[Region]] 1
    * @param f2 [[Region]] 2
    * @return The boolean value indicating compatibility
    */
  def isCompatible(f1: Region, f2: Region): Boolean = {
    if(f1 == Empty || f2 == Empty) false
    else isCompatible((f1.orientation, f2.orientation))
  }

  /** An arbitrary ordering on [[Orientation]]s. */
  object ArbitraryOrdering extends Ordering[Orientation] {
    override def compare(o1: Orientation, o2: Orientation): Int = {
      (o1, o2) match {
        case (x, y) if x == y => 0
        case (Plus, _) => -1
        case (_, Plus) => 1
        case (Unstranded, _) => 1
        case (Minus, _) => -1
        case (Both, Minus) => 1
        case (Both, _) => -1; case _ => throw new UnsupportedOperationException("Not implemented: " + o1 + " " + o2)
      }
    }
  }

}