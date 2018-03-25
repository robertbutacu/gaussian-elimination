package structures

case class Epsilon(precision: Int) {
  require(precision > 0 && precision < 16)
  val decimalsPower: Double = Math.pow(10, precision)

  val toNegative10: Double = Math.pow(10, - precision)
}
