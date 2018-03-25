package gaussian.elimination

import structures.{Epsilon, Matrix}

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

  def startAlgorithm[A: Numeric](matrix: Matrix[A], epsilon: Epsilon): Unit = {

  }
}
