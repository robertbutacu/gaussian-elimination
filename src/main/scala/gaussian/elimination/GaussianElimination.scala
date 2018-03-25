package gaussian.elimination

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j

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
  def searchForPivot(l: Int, tensor: INDArray): Option[Int] = {
    None
  }

  def interchangeRows(first: Int, second: Int, tensor: INDArray): INDArray = {
    Nd4j.zeros(1)
  }

  def startAlgorithm(tensor: INDArray): Unit = {
    def go(row: Int, tensor: INDArray): Unit = {
      val nextRow = row + 1
      val pivot = searchForPivot(row, tensor)
    }
  }
}
