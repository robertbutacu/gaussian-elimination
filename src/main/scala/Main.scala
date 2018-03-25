import gaussian.elimination.GaussianElimination
import structures.{Epsilon, RegularMatrix}

object Main extends App {
  val matrixTest = RegularMatrix[Double](List(
    List[Double](0.02, 0.01, 0, 0),
    List[Double](1, 2, 1, 0),
    List[Double](0, 1, 2, 1),
    List[Double](0, 0, 100, 200)
  ))

  val b = List[Double](0.02, 1, 4, 800)

  val solution = GaussianElimination.startAlgorithm(matrixTest, b, Epsilon(4))

  println(solution)
  solution.solve() match {
    case None => println("No solution")
    case Some(result) => println(result)
  }
}
