package util

import java.lang.Number

// This should be called Fractional since it allows a zero-valued denominator

/**
 * A class for doing arithmetic with rational numbers.
 */
case class Rational(var numerator: BigInt, var denominator: BigInt)
    extends Pair[BigInt, BigInt](numerator, denominator) with Ordered[Rational] {
  import Rational._
      
  def this(integer: BigInt) = this(integer, 1)
  
  def simplify(): Rational = 
    if (denominator == 0)
      if (numerator < 0) -INFINITY else INFINITY
    else {
      val d = numerator gcd denominator
      Rational(numerator / d, denominator / d)
    }
  
  def negate(): Rational = Rational(-numerator, denominator)
  
  def unary_-(): Rational = negate
  
  def doubleValue(): Double = numerator.doubleValue / denominator.doubleValue
  
  def floatValue(): Float = numerator.floatValue / denominator.floatValue
  
  def longValue(): Long = (numerator / denominator).longValue
  
  def intValue(): Int = (numerator / denominator).intValue
  
  override def compare(that: Rational): Int =
    (numerator * that.denominator - denominator * that.numerator).intValue
  
  override def equals(that: Any): Boolean = 
    that.isInstanceOf[Rational] && compare(that.asInstanceOf[Rational]) == 0
  
  def +(that: Rational): Rational = 
    Rational(numerator * that.denominator + denominator * that.numerator,
             denominator * that.denominator)
  
  def -(that: Rational): Rational = 
    Rational(numerator * that.denominator - that.numerator * denominator,
             denominator * that.denominator)
  
  def *(that: Rational): Rational =
    Rational(numerator * that.numerator,
             denominator * that.denominator)
  
  def /(that: Rational): Rational = 
    Rational(numerator * that.denominator,
             denominator * that.numerator)
  
  override def toString(): String = "(" + numerator + "%" + denominator + ")"
}

object Rational {
  val INFINITY = Rational(1, 0)
  val ZERO = Rational(0, 1)
  val ONE = Rational(1, 1)
  
  implicit def makeRational(numerator: BigInt) = new {
    def %/ (denominator: BigInt): Rational = Rational(numerator, denominator)
  }
}
