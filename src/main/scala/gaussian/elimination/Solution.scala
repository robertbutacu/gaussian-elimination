package gaussian.elimination

import structures.{Epsilon, Matrix}

case class Solution(matrix: Matrix[Double], b: List[Double], lastPivot: Int, epsilon: Epsilon){
  def isSingular: MatrixType = {
    if(Math.abs(matrix.rows(lastPivot).max) < epsilon.toNegative10 )
      Singular
    else
      NotSingular
  }

  def solve(): Option[List[Double]] = {
    isSingular match {
      case Singular => None
      case NotSingular => Some(solveSystem())
    }
  }

  private def solveSystem(): List[Double] = {
    /*
      1 2 3 4 5
      0 1 2 3 4
      0 0 1 2 3
      0 0 0 1 2
      0 0 0 0 1
      =>
      0 0 0 0 1
      0 0 0 1 2
      0 0 1 2 3
      0 1 2 3 4
      1 2 3 4 5
     */

    val reversedMatrix = matrix.rows.reverse.map(_.reverse)

    /*
      1
      1 2
      1 2 3
      1 2 3 4
      1 2 3 4 5
     */
    val removedZeroes = reversedMatrix.map(row => row.dropWhile(_ == 0.0))

    val zippedWithB = removedZeroes.zip(b.reverse)

    val result = zippedWithB.foldLeft(List.empty[Double]){(acc, curr) =>
      val result = curr._2
      val coefficient = curr._1.head
      val restOfEquation = curr._1.tail
      val leftSideSum = restOfEquation.zip(acc).foldRight(0.0){(curr, acc) =>
        println(curr._1 + "*" + curr._2)
        acc + curr._1 * curr._2
      }

      println("result " + result)
      println("coefficient " + coefficient)
      println("rest " + curr._1.tail)
      println("left side sum " + leftSideSum)
      println("result " + epsilon.truncate((result - leftSideSum) / coefficient))
      println("\n\n")

      epsilon.truncate((result - leftSideSum) / coefficient) +: acc
    }

    result
  }
}
