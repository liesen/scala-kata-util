package kata.digits

import scala.collection.mutable.{ Buffer, ListBuffer }

class Number(val digits: Buffer[Int]) {  
  // reverse
  
  def zipA(xs: Iterable[Int], ys: Iterable[Int]): List[(Int, Int)] = {
    if (xs.isEmpty && ys.isEmpty) {
      Nil
    } else if (xs.isEmpty) {
      (0, ys.elements.next) :: zipA(Nil, ys drop 1)
    } else if (ys.isEmpty) {
      (xs.elements.next, 0) :: zipA(xs drop 1, Nil)
    } else {
      (xs.elements.next, ys.elements.next) :: zipA(xs drop 1, ys drop 1)
    }
  
  }
  
  // +
  def +(that: Number): Number = {
    val ds: Buffer[Int] = new ListBuffer()
    
    zipA(digits, that.digits).foldLeft((0, ds)){ (a, xy) => 
      val _ @ (x, y) = xy
      val _ @ (carry, n) = a
      val d = x + y + carry
      
      if (d >= 10) {
        (d / 10, n + (d % 10))
      } else {
        (0, n + d)
      }
    } match {
      case (0, num)     => new Number(num)
      case (carry, num) => new Number(num.+:(carry))
    }
  }
  
  /** 
   * Increases the number by 1
   */
  def unary_++(): Number = {
    var i = 0
    var incremented = false
    
    while (i < digits.size && !incremented) {
      if (digits(i) == 9) {
        digits(i) = 0
      } else {
        digits(i) += 1
        incremented = true
      }
    }
    
    this
  }
  
  def isPalindrome(): Boolean = 
    digits.elements take (digits.size / 2) zip digits.reverse.elements forall { p => p._1 == p._2 }
  
  def reverse(): Number = new Number(new ListBuffer ++ digits.reverse)
  
  def intValue(): Int = {
    var p = 1
    var n = 0
    
    for (d <- digits) {
      n += d * p
      p *= 10
    }
    
    n
  }
  
  override def toString(): String = intValue.toString
}

object Number {
  val ZERO = fromDigits(0)
  
  val ONE = fromDigits(1)

  def fromDigits(ns: Int*) = new Number(new ListBuffer ++ ns.reverse)
}
