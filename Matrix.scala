package jecrevin.math

import jecrevin.math.Numerical.Implicits.infixNumericalOps

class Matrix[T: Numerical](values: Seq[T], rows: Int, cols: Int):
  val elems = values.toVector
  val shape = (rows, cols)

  def apply(r: Int, c: Int) = elems(r * shape(1) + c)

  def +(m: Matrix[T]) =
    require(m.shape == this.shape, "Matrixs' shape conflict!")
    Matrix(elems.zip(m.elems).map((x, y) => x + y), shape(0), shape(1))
  def +(x: T) = Matrix(elems.map(_ + x), shape(0), shape(1))

  def -(m: Matrix[T]) =
    require(m.shape == this.shape, "Matrixs' shape conflict!")
    Matrix(elems.zip(m.elems).map((x, y) => x - y), shape(0), shape(1))
  def -(x: T) = Matrix(elems.map(_ - x), shape(0), shape(1))

  def *(m: Matrix[T])(implicit num: Numerical[T]) =
    require(m.shape(0) == this.shape(1), "Matrixs' shape conflict!")
    Matrix(
      for
        r <- 0 until shape(0)
        c <- 0 until shape(1)
      yield row(r).zip(m.col(c)).map((x, y) => x * y).reduce(num.plus),
      shape(0),
      m.shape(1)
    )

  def traspose = Matrix(
    for
      c <- 0 until shape(1)
      r <- 0 until shape(0)
    yield apply(r, c),
    shape(1),
    shape(0)
  )

  def row(r: Int) = elems.slice(r * shape(1), r * shape(1) + shape(1))

  def col(c: Int) = (for r <- 0 until shape(0) yield apply(r, c)).toVector

  def rowsExchanged(r1: Int, r2: Int) = Matrix(
    for
      r <- 0 until shape(0)
      c <- 0 until shape(1)
    yield r match
      case `r1` => apply(r2, c)
      case `r2` => apply(r1, c)
      case _    => apply(r, c)
    ,
    shape(0),
    shape(1)
  )

  def colsExchanged(c1: Int, c2: Int) = Matrix(
    for
      r <- 0 until shape(0)
      c <- 0 until shape(1)
    yield c match
      case `c1` => apply(r, c2)
      case `c2` => apply(r, c1)
      case _    => apply(r, c)
    ,
    shape(0),
    shape(1)
  )

  def rowUpdated(values: Seq[T], i: Int) =
    require(
      values.length == this.shape(1),
      "Row's length is conflicted with matrix's shape!"
    )
    Matrix(
      for
        r <- 0 until shape(0)
        c <- 0 until shape(1)
      yield r match
        case `i`    => values(c)
        case _: Int => apply(r, c)
      ,
      shape(0),
      shape(1)
    )

  def colUpdated(values: Seq[T], i: Int) =
    require(
      values.length == this.shape(0),
      "Column's length is conflicted with matrix's shape!"
    )
    Matrix(
      for
        r <- 0 until shape(0)
        c <- 0 until shape(1)
      yield c match
        case `i`    => values(r)
        case _: Int => apply(r, c)
      ,
      shape(0),
      shape(1)
    )

  override def toString(): String =
    var str = "Matrix["
    for i <- 0 until shape(0) do
      str += "\n  %.2f".format(apply(i, 0))
      for x <- row(i).tail do str += "%8.2f".format(x)
    str + "]"

  override def equals(x: Any): Boolean = x match
    case m: Matrix[?] =>
      if m.shape == this.shape && m.elems == this.elems then true else false
    case _ => false

  override def hashCode(): Int = elems.hashCode().abs - shape.hashCode().abs

object Matrix:
  def zeros(rows: Int, cols: Int): Matrix[Float] = Matrix(
    Vector.fill(rows * cols)(0f),
    rows,
    cols
  )

  def zeros(dimen: Int): Matrix[Float] = zeros(dimen, dimen)

  def identityMat(dimen: Int) = Matrix(
    for
      r <- 0 until dimen
      c <- 0 until dimen
    yield c match
      case `r` => 1f
      case _   => 0f
    ,
    dimen,
    dimen
  )

  implicit def matFloat2Double(m: Matrix[Float]): Matrix[Double] = Matrix(
    m.elems.map(_.toDouble),
    m.shape(0),
    m.shape(1)
  )

  object Implicits:
    implicit def matFloat2Complex(m: Matrix[Float]): Matrix[Complex[Float]] =
      Matrix(
        m.elems.map(Complex(_, 0)),
        m.shape(0),
        m.shape(1)
      )
    implicit def matDouble2Complex(m: Matrix[Double]): Matrix[Complex[Double]] =
      Matrix(
        m.elems.map(Complex(_, 0)),
        m.shape(0),
        m.shape(1)
      )
