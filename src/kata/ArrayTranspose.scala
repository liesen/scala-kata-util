package kata

object ArrayTranspose {
  def transpose[A](matrix: Array[Array[A]]): Array[Array[A]] = {
    var transposed: Array[Array[A]] = new Array(matrix(0).size, matrix.size)
    
    for (row <- 0 until matrix.size; col <- 0 until matrix(row).size) {
      transposed(col)(row) = matrix(row)(col)
    }
    
    transposed
  }
}
