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
  def searchForPivot[A: Numeric](l: Int, tensor: Matrix[A]): Option[Int] = {
    None
  }

  def startAlgorithm[A: Numeric](matrix: Matrix[A], b: List[A], epsilon: Epsilon): Unit = {
    val n = implicitly[Fractional[A]]

    def isPivotNull(currentColumn: Int, matrix: Matrix[A]): Boolean =
      n.toDouble(n.abs(matrix(0)(currentColumn))) < epsilon.value

    @tailrec
    def gaussianElimination(currentColumn: Int, matrix: Matrix[A], b: List[A]): Matrix[A] = {
      if (currentColumn == matrix.rowLength - 1 || isPivotNull(currentColumn, matrix))
        matrix
      else {
        //getting coefficients of the division
        val coefficients = for {
          row <- matrix.rows.drop(currentColumn)
          currentRow = matrix.rows(currentColumn)
          coefficient = n.div(row(currentColumn), currentRow(currentColumn))
        } yield coefficient

        //rows from which matrix's rows will be subtracted from
        val rowsForSubtraction = coefficients.map(c => matrix.rows(currentColumn).map(n.times(_ , c)))

        val transformedMatrix = RegularMatrix(matrix.rows.slice(0, currentColumn + 1) :::
        matrix.rows.slice(currentColumn + 1, matrix.N).zip(rowsForSubtraction).map{
          pair =>
            pair._1.zip(pair._2).map(p => truncate(n.minus(p._1, p._2), epsilon))
        })

        val pivot = transformedMatrix.maxByColumn(currentColumn)
        val pivotFirstMatrix = transformedMatrix.swapRows(currentColumn, pivot)
        val pivotFirstB = swapElements(b, currentColumn, pivot)
        gaussianElimination(currentColumn + 1, pivotFirstMatrix, pivotFirstB)
      }
    }

    val finalMatrix = gaussianElimination(0, matrix, b)
  }

  private def swapElements[A: Numeric](list: List[A], first: Int, second: Int): List[A] = {
    def inBoundaries(i: Int): Boolean = i >= 0 && i < list.length

    require(inBoundaries(first) && inBoundaries(second))

    list.updated(first, list(second)).updated(second, list(first))
  }

  private def truncate[A: Numeric](a: A, epsilon: Epsilon): A = {
    val n = implicitly[Numeric[A]]
    n(Math.floor(n.toDouble(a) * epsilon.value) / epsilon.value)
  }
}
