package kata.primes

/**
 * Implementation of the Sieve of Atkins
 */
object Atkins {
  import scala.collection.mutable.{ BitSet, Buffer, ListBuffer }
  
  def sieve(max: Int): Buffer[Int] = {
    var primes = new ListBuffer + 2 + 3
    
    if (max <= 3) {
      return primes
    }
    
    val sieve = new BitSet(max / 2 + 1)
    val max1 = max + 1
    var xmax = Math.sqrt(max / 4.0).toInt
    var x2 = 0
    
    for (xd <- 4 until 8 * xmax + 2 by 8) {
      x2 += xd
      
      val ymax = Math.sqrt(max1 - x2).toInt
      var n = x2 + ymax * ymax
      var nd = ymax * 2 - 1
      
      if ((n & 1) == 0) {
        n -= nd
        nd -= 2
      }
      
      for (d <- (nd - 1) * 2 until -1 by -8) {
        val m = n % 12
        
        if (m == 1 || m == 5) {
          sieve(n / 2) = !sieve(n / 2)
        }
        
        n -= d
      }
    }
      
    xmax = Math.sqrt(max / 3.0).toInt
    x2 = 0
    
    for (xd <- 3 until 6 * xmax + 2 by 6) {
      x2 += xd
      
      val ymax = Math.sqrt(max1 - x2).toInt
      var n = x2 + ymax * ymax
      var nd = ymax * 2 - 1
      
      if ((n & 1) == 0) {
        n -= nd
        nd -= 2
      }
      
      for (d <- (nd - 1) * 2 to 0 by -8) {
        if (n % 12 == 7) {
          sieve(n / 2) = !sieve(n / 2)
        }
        
        n -= d
      }
    }
      
    xmax = (1 + Math.sqrt(2 * max + 1)) / 2 
    x2 = 0
    var ymin = -1
    var xd = 3
    
    for (x <- 1 until xmax + 1) {
      x2 += xd
      xd += 6
      
      if (x2 > max) {
        ymin = 4 * Math.ceil(Math.sqrt(x2 - max1)).toInt - 8
      }
      
      var n = 2 * (x * x + x) - 1
      
      for (d <- 4 * x - 8 until ymin by -8) {
        if (n % 12 == 11) {
          sieve(n / 2) = !sieve(n / 2)
        }
        
        n += d
      }
    }
    
    for (n <- 2 to Math.sqrt(max1).toInt / 2 if (sieve(n))) {
      val p = n * 2 + 1
      primes += p
      
      for (k <- p * p until max1 by 2 * p * p) {
        sieve(k / 2) = false
      }
    }
    
    var s = Math.sqrt(max1).toInt + 1
    
    if (s % 2 == 0) {
      s += 1
    }
    
    return primes ++ ((s until max1 by 2) filter { i => sieve(i / 2) })
  }
}
