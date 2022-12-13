package jecrevin.math

import math.{atan2, hypot, pow}
import math.Numeric.Implicits.infixNumericOps
import jecrevin.math.Numerical.Implicits.infixNumericalOps

class Complex[T: RealNum](val re: T, val im: T):
  val phase = atan2(re.toDouble, im.toDouble)
  val amp   = hypot(re.toDouble, im.toDouble)
  val norm  = re * re + im * im

  override def equals(x: Any): Boolean =
    x match
      case c: Complex[?] =>
        if c.re == re && c.im == im then true
        else false
      case _: Any => false

  override def hashCode(): Int = re.hashCode().abs - im.hashCode().abs

  override def toString(): String = s"$re + i$im"

  def unary_- : Complex[T]         = Complex[T](-re, -im)
  def conjugate: Complex[T]        = Complex[T](re, -im)
  def +(x: Complex[T]): Complex[T] = Complex[T](re + x.re, im + x.im)
  def -(x: Complex[T]): Complex[T] = this + -x
  def *(x: Complex[T]): Complex[T] =
    Complex[T](re * x.re - im * x.im, re * x.im + im * x.re)
  def /(x: T): Complex[T] =
    require(x != 0, "Divided by zero!")
    Complex[T](re / x, im / x)
  def /(x: Complex[T]): Complex[T] =
    this * x.conjugate / x.norm

object Complex:
  def zero: Complex[Float] = Complex(0, 0)
  def one: Complex[Float]  = Complex(1, 0)
  def im: Complex[Float]   = Complex(0, 1)

  implicit def complexFloat2Double(cf: Complex[Float]): Complex[Double] =
    Complex(cf.re, cf.im)
  implicit def fromNumeric2CF[T: Numeric](x: T): Complex[Float] =
    Complex(x.toFloat, 0f)
  implicit def fromNumeric2CD[T: Numeric](x: T): Complex[Double] =
    Complex(x.toDouble, 0d)
