package io.skadi

/**
  * Type class that defines an isomorphism between types `From` and `To`.
  *
  * Used as a simplified version of `CanBuildFrom` or `BuildFrom` for span context encoding & decoding
  */
trait AsCarrier[From, To] {

  def from(map: From): To
  def to(carrier: To): From

}

object AsCarrier {

  implicit val mapIdentity: AsCarrier[Map[String, String], Map[String, String]] =
    new AsCarrier[Map[String, String], Map[String, String]] {
      def from(map: Map[String, String]): Map[String, String] = map

      def to(carrier: Map[String, String]): Map[String, String] = carrier
    }

}
