package util

object Combinatorics {
  def factorial(n: Int): BigInt = n match {
    case 0           => BigInt(1)
    case n if n >= 1 => (2 to n).foldLeft(BigInt(1)){ _ * _ }
    case _           => error("Can not calculate the factorial of a negative number: " + n)
  }
  
  def choose(n: Int, k: Int): BigInt = (n, k) match {
    case (_, 1) => n
    case _      if k > n / 2 => choose(n, n - k) 
    case _      => (n - k + 1 to n).foldLeft(BigInt(1)){ _ * _ } / factorial(k) // No reduction whatsoever
  }
}
