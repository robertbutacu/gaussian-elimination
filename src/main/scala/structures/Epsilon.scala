package structures

case class Epsilon(precision: Int) {
  def apply(coefficients: List[Double]) = {
    coefficients.map(c => Math.floor(c * this.decimalsPower) / this.decimalsPower)
  }

  require(precision > 0 && precision < 16)
  val decimalsPower: Double = Math.pow(10, precision)

  val toNegative10: Double = Math.pow(10, - precision)
}
