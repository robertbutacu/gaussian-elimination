package structures

class Epsilon(precision: Int) {
  require(precision > 0 && precision < 16)
  val value: Double = Math.pow(10, precision)
}
