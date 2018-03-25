import structures.RegularMatrix


object Main extends App {
  val matrix = RegularMatrix((1 to 9).map(i => List.fill(9)(i)).toList)

  val matrix2 = RegularMatrix((1 to 9).map(i => (1 to 9).toList).toList)

  println(matrix.swapRows(0, 0).swapRows(0, 0).swapRows(0, 0))
}
