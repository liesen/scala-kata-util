package kata

object PythagoreanTriplets {
  type Triple = (Int, Int, Int)
  
  def U(triple: Triple): Triple = {
    val _ @ (x, y, z) = triple
    (x - 2 * y + 2 * z, 2 * x - y + 2 * z, 2 * x - 2 * y + 3 * z)
  }
  
  def A(triple: Triple): Triple = {
    val _ @ (x, y, z) = triple
    (x + 2 * y + 2 * z, 2 * x + y + 2 * z, 2 * x + 2 * y + 3 * z)
  }
  
  def D(triple: Triple): Triple = {
    val _ @ (x, y, z) = triple
    (-x + 2 * y + 2 * z, -2 * x + y + 2 * z, -2 * x + 2 * y + 3 * z)
  }
}
