package jecrevin.math

object Numerical:
  @inline
  def apply[T](implicit num: Numerical[T]) = num

  trait ExtraImplicits:
    implicit def infixNumericalOps[T](lhs: T)(implicit
        num: Numerical[T]
    ): Numerical[T]#NumericalOps =
      new num.NumericalOps(lhs)

  object Implicits extends ExtraImplicits

  trait FloatIsNumerical extends RealNum[Float]:
    override def plus(x: Float, y: Float): Float     = x + y
    override def minus(x: Float, y: Float): Float    = x - y
    override def times(x: Float, y: Float): Float    = x * y
    override def div(x: Float, y: Float): Float      = x / y
    override def negate(x: Float): Float             = -x
    override def conjugate(x: Float): Float          = x
    override def toFloat(x: Float): Float            = x
    override def toDouble(x: Float): Double          = x.toDouble
    override def toComplex(x: Float): Complex[Float] = Complex(x, 0)
    override def abs(x: Float): Float                = x.abs
    override def zero: Float                         = 0f
    override def one: Float                          = 1f

  implicit object FloatIsNumerical
      extends FloatIsNumerical
      with Ordering.Float.IeeeOrdering

  trait DoubleIsNumerical extends RealNum[Double]:
    def plus(x: Double, y: Double): Double    = x + y
    def minus(x: Double, y: Double): Double   = x - y
    def times(x: Double, y: Double): Double   = x * y
    def div(x: Double, y: Double): Double     = x / y
    def negate(x: Double): Double             = -x
    def conjugate(x: Double): Double          = x
    def toFloat(x: Double): Float             = x.toFloat
    def toDouble(x: Double): Double           = x
    def toComplex(x: Double): Complex[Double] = Complex(x, 0)
    def abs(x: Double): Double                = x.abs
    def zero: Double                          = 0d
    def one: Double                           = 1d

  implicit object DoubleIsNumerical
      extends DoubleIsNumerical
      with Ordering.Double.IeeeOrdering

  trait ComplexFloatIsNumerical extends Numerical[Complex[Float]]:
    override def plus(x: Complex[Float], y: Complex[Float]): Complex[Float] =
      x + y
    override def minus(x: Complex[Float], y: Complex[Float]): Complex[Float] =
      x - y
    override def times(x: Complex[Float], y: Complex[Float]): Complex[Float] =
      x * y
    override def div(x: Complex[Float], y: Complex[Float]): Complex[Float] =
      x / y
    override def negate(x: Complex[Float]): Complex[Float]    = -x
    override def conjugate(x: Complex[Float]): Complex[Float] = x.conjugate
    override def toFloat(x: Complex[Float]): Float            = x.re
    override def toDouble(x: Complex[Float]): Double          = x.re.toDouble
    override def toComplex(x: Complex[Float]): Complex[Float] = x
    override def zero: Complex[Float]                         = Complex.zero
    override def one: Complex[Float]                          = Complex.one

  implicit object ComplexFloatIsNumerical extends ComplexFloatIsNumerical

  trait ComplexDoubleIsNumerical extends Numerical[Complex[Double]]:
    override def plus(x: Complex[Double], y: Complex[Double]): Complex[Double] =
      x + y
    override def minus(
        x: Complex[Double],
        y: Complex[Double]
    ): Complex[Double] = x - y
    override def times(
        x: Complex[Double],
        y: Complex[Double]
    ): Complex[Double] = x * y
    override def div(x: Complex[Double], y: Complex[Double]): Complex[Double] =
      x / y
    override def negate(x: Complex[Double]): Complex[Double]    = -x
    override def conjugate(x: Complex[Double]): Complex[Double] = x.conjugate
    override def toFloat(x: Complex[Double]): Float             = x.re.toFloat
    override def toDouble(x: Complex[Double]): Double           = x.re
    override def toComplex(x: Complex[Double]): Complex[Double] = x
    override def one: Complex[Double]                           = Complex.one
    override def zero: Complex[Double]                          = Complex.zero

  implicit object ComplexDoubleIsNumerical extends ComplexDoubleIsNumerical

trait Numerical[T]:
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def div(x: T, y: T): T
  def negate(x: T): T
  def conjugate(x: T): T
  def toFloat(x: T): Float
  def toDouble(x: T): Double
  def toComplex(x: T): Complex[?]

  def zero: T
  def one: T

  class NumericalOps(lhs: T):
    def +(rhs: T) = plus(lhs, rhs)
    def -(rhs: T) = minus(lhs, rhs)
    def *(rhs: T) = times(lhs, rhs)
    def /(rhs: T) = div(lhs, rhs)
    def unary_-   = negate(lhs)
    def conjugate = Numerical.this.conjugate(lhs)
    def toFloat   = Numerical.this.toFloat(lhs)
    def toDouble  = Numerical.this.toDouble(lhs)
    def toComplex = Numerical.this.toComplex(lhs)
    def zero      = Numerical.this.zero
    def one       = Numerical.this.one

  implicit def mkNumericalOps(lhs: T): NumericalOps = new NumericalOps(lhs)
