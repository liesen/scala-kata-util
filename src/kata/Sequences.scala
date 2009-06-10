package kata


/**
 * Contains some good-to-have number sequences.
 */
object Sequences {
  import Numbers._
  import Rational._
  
  def farey(n: Int): Iterator[Rational] = new Iterator[Rational] {
    var a = 0
    var b = 1
    var c = 1
    var d = n
    var r = new Rational(a, b)
    
    def hasNext = c < n
    
    def next = {
      val k = (n + b) / d
      val e = k * c - a
      val f = k * d - b
      
      a = c
      b = d
      c = e
      d = f
      
      r.numerator = a
      r.denominator = b
      
      r
    }
  }
  
  /**
   * Iterator for the Fibonacci number sequence.
   */
  val fibonacci = new Iterator[BigInt] {
    private[this] var previous = (BigInt(0), BigInt(1))
    
    val hasNext = true
    
    def next = {
      previous = (previous._2, previous._1 + previous._2)
      previous._1
    }
  }
}