package gaussian.elimination

import structures.{Epsilon, Matrix}

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
    @tailrec
    def gaussianElimination(currentColumn: Int, matrix: Matrix[A], b: List[A]): Matrix[A] = {
      if(currentColumn == matrix.rowLength - 1)
        matrix
      else {
        val pivot = matrix.maxByColumn(currentColumn)
        val pivotFirstMatrix = matrix.swapRows(0, pivot)
        val pivotFirstB = swapElements(b, 0, pivot)
        gaussianElimination(currentColumn + 1, pivotFirstMatrix, pivotFirstB)
      }
    }

    val finalMatrix = gaussianElimination(0, matrix, b)
  }

  def swapElements[A: Numeric](list: List[A], first: Int, second: Int): List[A] = {
    def inBoundaries(i: Int): Boolean = i >= 0 && i < list.length
    require(inBoundaries(first) && inBoundaries(second))

    list.updated(first, list(second)).updated(second, list(first))
  }
}
