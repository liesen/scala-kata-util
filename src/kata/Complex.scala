package kata

abstract class Complex {
  def real: Double
  def imag: Double
  def magnitude: Double
  def angle: Double

  def +(that: Complex) = new RectComplex(real + that.real, imag + that.imag)
  def -(that: Complex) = new RectComplex(real - that.real, imag - that.imag)

  def *(that: Complex) = new PolarComplex(magnitude * that.magnitude, angle + that.angle)
  def /(that: Complex) = new PolarComplex(magnitude / that.magnitude, angle - that.angle)

  def conjugate = new RectComplex(real, -imag)
}

object Complex {
  val i = new RectComplex(0, 1)

  implicit def Int2Complex(n: Int): Complex = new RectComplex(n, 0)
}

class RectComplex(val real: Double, val imag: Double) extends Complex {
  val magnitude = Math.sqrt(real * real + imag * imag)
  val angle = Math.atan(imag / real)
}

class PolarComplex(val magnitude: Double, val angle: Double) extends Complex {
  val real = magnitude * Math.cos(angle)
  val imag = magnitude * Math.sin(angle)
}


