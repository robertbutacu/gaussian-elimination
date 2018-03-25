package gaussian.elimination

import structures.{Epsilon, Matrix, RegularMatrix}

import scala.annotation.tailrec

object GaussianElimination {
  /*
  l = 1;
  cauta pivot();
  interschimba linii(); // (dac˘a e cazul)
  while (l ≤ n − 1) and (|all| > ) do
  begin
  (5)+(6)+(7);
  l = l + 1;
  cauta pivot();
  interschimba linii(); // (dac˘a e cazul)
  end
  if |all| ≤  then print(’matrice singulara’);
  else
  begin
  rezolva sistem superior triunghiular();
  // (se folose¸ste metoda substitit¸iei inverse)
  verifica solutie();
  end

   */
  def startAlgorithm(matrix: Matrix[Double], b: List[Double], epsilon: Epsilon): Solution = {
    def isPivotNotNull(currentColumn: Int, matrix: Matrix[Double]): Boolean =
      Math.abs(matrix.rows(currentColumn).max) > epsilon.toNegative10

    @tailrec
    def gaussianElimination(currentColumn: Int, matrix: Matrix[Double], b: List[Double]): Solution = {
      if (currentColumn == matrix.rowLength - 1 && isPivotNotNull(currentColumn, matrix))
        Solution(matrix, b, currentColumn, epsilon)
      else {
        //getting coefficients of the division
        val coefficients = for {
          row <- matrix.rows.drop(currentColumn + 1)
          currentRow = matrix.rows(currentColumn)
          coefficient = row(currentColumn) / currentRow(currentColumn)
        } yield coefficient

        val trimmedCoefficients = epsilon.apply(coefficients)

        val transformedMatrix = transformMatrix(currentColumn, matrix, trimmedCoefficients, epsilon)

        val transformedB = transformB(currentColumn, b, trimmedCoefficients, epsilon)

        val pivot = transformedMatrix.maxByColumn(currentColumn)
        val pivotFirstMatrix = transformedMatrix.swapRows(currentColumn, pivot)
        val pivotFirstB = swapElements(transformedB, currentColumn, pivot)

        gaussianElimination(currentColumn + 1, pivotFirstMatrix, pivotFirstB)
      }
    }
    val pivot = matrix.maxByColumn(0)
    val pivotFirstMatrix = matrix.swapRows(0, pivot)
    val pivotFirstB = swapElements(b, 0, pivot)

    gaussianElimination(0, pivotFirstMatrix, pivotFirstB)
  }

  private def transformB(currentColumn: Int,
                                     b: List[Double],
                                     coefficients: List[Double],
                                     epsilon: Epsilon): List[Double] = {
    println("before")
    println(b)
    println(coefficients)
    val result = b.slice(0, currentColumn + 1) :::
      b.slice(currentColumn + 1, b.length)
        .zip(coefficients)
        .map(p => truncate(p._1 - ( p._1 * p._2), epsilon))
    println("after")
    println(result)
    result
  }

  private def transformMatrix(currentColumn: Int,
                                          matrix: Matrix[Double],
                                          coefficients: List[Double],
                                          epsilon: Epsilon): Matrix[Double] = {
    //rows from which matrix's rows will be subtracted from
    val rowsForSubtraction = coefficients.map(c => matrix.rows(currentColumn).map(_ * c))

    RegularMatrix(
      matrix.rows.slice(0, currentColumn + 1) //first slice which remains unchanged
        //zipping with rows which will be subtracted from one another
        //TODO maybe beautiful version ?
        ::: matrix.rows.slice(currentColumn + 1, matrix.N).zip(rowsForSubtraction).map {
        pair =>
          //zipping elements of the rows
          pair._1.zip(pair._2).map(p => truncate(p._1 - p._2, epsilon))
      })
  }

  private def swapElements[A: Fractional](list: List[A], first: Int, second: Int): List[A] = {
    def inBoundaries(i: Int): Boolean = i >= 0 && i < list.length

    require(inBoundaries(first) && inBoundaries(second))

    list.updated(first, list(second)).updated(second, list(first))
  }

  private def truncate(a: Double, epsilon: Epsilon): Double = {
    Math.floor(a * epsilon.decimalsPower) / epsilon.decimalsPower
  }
}
