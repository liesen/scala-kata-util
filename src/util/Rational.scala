package util

import java.lang.Number

// This should be called Fractional since it allows a zero-valued denominator

/**
 * A class for doing arithmetic with rational numbers.
 */
case class Rational(var numerator: BigInt, var denominator: BigInt)
    extends Pair[BigInt, BigInt](numerator, denominator) with Ordered[Rational] {
  def this(integer: BigInt) = this(integer, 1)
  
  def simplify(): Rational = gcd(this)
  
  def negate(): Rational = Rational(-numerator, denominator)
  
  def doubleValue(): Double = numerator.doubleValue / denominator.doubleValue
  
  def floatValue(): Float = (numerator / denominator).floatValue
  
  def longValue(): Long = (numerator / denominator).longValue
  
  def intValue(): Int = (numerator / denominator).intValue
  
  override def compare(that: Rational): Int =
    (numerator * that.denominator - denominator * that.numerator).intValue
  
  override def equals(that: Any): Boolean = that.isInstanceOf[Rational] && compare(that.asInstanceOf[Rational]) == 0
  
  def /(that: Rational): Rational = Rational(numerator * that.denominator, denominator * that.numerator)
  
  def *(that: Rational): Rational = Rational(numerator * that.numerator, denominator * that.denominator)
  
  def +(that: Rational): Rational = Rational(numerator * that.denominator + denominator * that.numerator, denominator * that.denominator)
  
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

final object gcd extends PartialFunction[Rational, Rational] { 
  def isDefinedAt(rational: Rational): Boolean = rational.denominator != 0
  
  def apply(rational: Rational): Rational = {
    val d = rational.numerator gcd rational.denominator
    Rational(rational.numerator / d, rational.denominator / d)
  }
}