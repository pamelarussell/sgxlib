package feature

/** Exceptions for invalid [[Feature]]s */
object Exceptions {

  /** Exception indicating invalid CDS size */
  class CDSSizeException(message: String) extends IllegalArgumentException(message)

}

