package util

/**
 * Integer partitions
 * ==================
 * 
 * zs1, zs2 are from the paper
 * 
 *   Fast algorithms for generating integer partitions
 *     Intern. J. Computer Math., Vol- 70. pp. 319-332
 *     A. Zoghbi, I. Stojmenovic
 */
object Partitions {
  // the second parameter will be called for each integer partition
  def zs1(n: Int)(output: Array[Int] => Any) = {
    val x = Array.make(n + 1, 1)
    
    x(1) = n
    var m = 1
    var h = 1
    var count = 1
    
    output(x.slice(1, 2))
    
    while (x(1) != 1) {
      if (x(h) == 2) {
        m += 1
        x(h) = 1
        h -= 1
      } else {
        val r = x(h) - 1
        var t = m - h + 1
        x(h) = r
        
        while (t >= r) {
          h += 1
          x(h) = r
          t -= r
        }
        
        if (t == 0) {
          m = h
        } else {
          m = h + 1
          
          if (t > 1) {
            h = h + 1
            x(h) = t
          }
        }
      }
      
      output(x.slice(1, m + 1))
    }
  }
  
  /** Generates partitions in lexicographic order. */
  def zs2(n: Int)(output: Array[Int] => Any): Unit = {
    val x = Array.make(n + 1, 1)
    
    output(x drop 1)
    
    x(0) = -1
    x(1) = 2
    var h = 1
    var m = n - 1
    
    output(x.slice(1, m + 1))
    
    while (x(1) != n) {
      if (m - h > 1) {
        h = h + 1
        x(h) = 2
        m = m - 1
      } else {
        var j = m - 2
        
        while (x(j) == x(m - 1)) {
          x(j) = 1
          j -= 1
        }
        
        h = j + 1
        x(h) = x(m - 1) + 1
        
        val r = x(m) + x(m - 1) * (m - h - 1)
        x(m) = 1
        
        if (m - h > 1) {
          x(m - 1) = 1
        }
        
        m = h + r - 1
      }
      
      output(x.slice(1, m + 1))
    }
  }
  
  def p(k: Long, n: Long): Long = {
    if (k > n)
      0
    else if (k == 0)
      0
    else if (k == n)
      1
    else
      p(k - 1, n - 1) + p(k, n - k)
  }
}
