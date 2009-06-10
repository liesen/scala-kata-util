package kata

sealed case class ContinuedFraction(val intValue: Int, val qs: Iterator[(Int, Int, Int)]) {
  def quotients(): Iterator[Int] = qs map { _._1 }
  
  def partialValues(n: Int): Stream[Rational] = {
    var a0 = new Rational(intValue)
    var stream = Stream(a0)

    for (_ <- 0 until n) {
      val (a, q, b) = qs.next
      a0 += (Rational.ONE / Rational(a, q))
      stream = a0 lazy_:: stream
    }

    stream
  }
}

object ContinuedFractions {
  // ContinuedFraction(Int, Repeating(Seq[Int]) or Seq[Int] or (Seq, Repeating)
  
  def squareRoot(n: Int): ContinuedFraction = {
    val intValue = Math.sqrt(n).toInt
    
    new ContinuedFraction(intValue, new Iterator[(Int, Int, Int)] {    
      private var a = intValue
      private var q = 1
      private var b = -intValue
      
      /** no more numbers if abs(sqrt(n)) ** 2 == n */
      val hasNext = a * a != n
    
      def next(): (Int, Int, Int) = {      
        val c = n - (b * b)      
        q = c / q
        a = (intValue - b) / q
        b = -(q * a + b)      
        (a, q, b)
     }})
  }
}
