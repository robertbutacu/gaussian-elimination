import gaussian.elimination.GaussianElimination
import structures.{Epsilon, RegularMatrix}
import scala.Fractional.Implicits._

object Main extends App {
  val matrixTest = RegularMatrix[Double](List(
    List[Double](25, 5, 1),
    List[Double](64, 8, 1),
    List[Double](144, 12, 1)
  ))

  val b = List[Double](106.7, 177.2, 279.2)

  println(GaussianElimination.startAlgorithm(matrixTest, b, Epsilon(2)))
}
