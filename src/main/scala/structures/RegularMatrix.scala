package structures

case class RegularMatrix[A: Numeric](rows: List[List[A]]) extends Matrix[A] {
  require(this.rows.forall(_.length == this.rows.maxBy(_.length).length))

  override def rowLength: Int = this.rows.head.length

  override def N: Int = this.rows.length

  override def M: Int = this.rowLength

  override def +++(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.plus)
  }

  override def ---(other: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    require(this.rowLength == other.rowLength)

    applyOperation(other, n.minus)
  }

  private def applyOperation(other: Matrix[A], f: (A, A) => A): Matrix[A] = {
    RegularMatrix[A](this.rows, other, f)
  }

  override def map[B](f: A => B)(implicit n: Numeric[B]): RegularMatrix[B] = RegularMatrix(this.rows.map(_.map(f)))(n)

  override def mapRows[B](f: List[A] => List[B])(implicit n: Numeric[B]): RegularMatrix[B] =
    RegularMatrix(this.rows.map(f))(n)

  override def swapRows(first: Int, second: Int): Matrix[A] = {
    require(RegularMatrix.isInBoundaries(first, N)
      && RegularMatrix.isInBoundaries(second, N))

    if (first == second)
      this
    else
      RegularMatrix(
        this.rows.slice(0, first)
          ::: List(this.rows(second))
          ::: this.rows.slice(first + 1, second)
          ::: List(this.rows(first))
          ::: this.rows.slice(second + 1, this.N))
  }

  override def maxByColumn(columnIndex: Int): Int =
    this.rows.slice(columnIndex + 1, this.N).zipWithIndex.maxBy { column =>
      val n = implicitly[Numeric[A]]
      n.abs(column._1(columnIndex))
    }._2
}

object RegularMatrix {
  def apply[A](rows: List[List[A]],
               other: Matrix[A],
               f: (A, A) => A)(implicit n: Numeric[A]): RegularMatrix[A] =
    new RegularMatrix(rows
      .zip(other.rows)
      .map {
        p =>
          p._1.zip(p._2)
            .map(v => f(v._1, v._2))
      }
    )(n)

  def isInBoundaries(row: Int, rowLength: Int): Boolean = {
    row >= 0 && row < rowLength
  }
}