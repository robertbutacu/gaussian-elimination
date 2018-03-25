package gaussian.elimination

import structures.{Epsilon, Matrix}

case class Solution(matrix: Matrix[Double], b: List[Double], lastPivot: Int, epsilon: Epsilon){
  def isSingular: MatrixType = {
    if(Math.abs(matrix.rows(lastPivot).max) < epsilon.toNegative10 )
      Singular
    else
      NotSingular
  }
}
