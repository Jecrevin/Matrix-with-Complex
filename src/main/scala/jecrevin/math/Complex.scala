package jecrevin.math

import math.{atan2, hypot, pow}

class Complex(val re: Double, val im: Double):
  val phase = atan2(re, im)
  val amp   = hypot(re, im)
  val norm  = pow(re, 2) + pow(im, 2)

  override def equals(x: Any): Boolean =
    x match
      case c: Complex =>
        if c.re == re && c.im == im then true
        else false
      case _: Any => false

  override def hashCode(): Int = re.hashCode().abs - im.hashCode().abs

  override def toString(): String = s"$re + i$im"

  def unary_- : Complex      = Complex(-re, -im)
  def conjugate: Complex     = Complex(re, -im)
  def +(x: Complex): Complex = Complex(re + x.re, im + x.im)
  def -(x: Complex): Complex = this + -x
  def *(x: Complex): Complex =
    Complex(re * x.re - im * x.im, re * x.im + im * x.re)
  def /(x: Double): Complex =
    require(x != 0, "Divided by zero!")
    Complex(re / x, im / x)
  def /(x: Complex): Complex = this * x.conjugate / x.norm

object Complex:
  def zero: Complex = Complex(0, 0)
  def one: Complex  = Complex(1, 0)
  def im: Complex   = Complex(0, 1)
  implicit def fromNumeric[T](x: T)(implicit op: Numeric[T]): Complex =
    Complex(op.toDouble(x), 0)
