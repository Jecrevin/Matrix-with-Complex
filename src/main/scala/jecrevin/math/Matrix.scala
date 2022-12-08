package jecrevin.math

/** abstract class  
 * @param elems
  * @param shape
  */
abstract class Matrix[T](
    final val elems: Vector[T],
    final val shape: Tuple2[Int, Int]
):
  require(
    elems.length == shape(0) * shape(1),
    "The number of elements is out of bound with shape!"
  )

  def ++(x: Matrix[T]): Matrix[T]
  def --(x: Matrix[T]): Matrix[T]
  def **(x: Matrix[T]): Matrix[T]
  def transpose: Matrix[T]

  def +(x: T): Matrix[T]
  def -(x: T): Matrix[T]
  def *(x: T): Matrix[T]
  def /(x: T): Matrix[T]

  final def row(r: Int): Vector[T] =
    (for i <- 0 until shape(1) yield elems(r * shape(1) + i)).toVector
  final def col(c: Int): Vector[T] =
    (for i <- 0 until shape(0) yield elems(c + i * shape(1))).toVector
  def rowsExchanged(i: Int, j: Int): Matrix[T]
  def colsExchanged(i: Int, j: Int): Matrix[T]
  def rowInserted(row: Seq[T], i: Int = this.shape(0)): Matrix[T]
  def colInserted(col: Seq[T], i: Int = this.shape(1)): Matrix[T]

  override def equals(x: Any): Boolean =
    x match
      case mc: Matrix[?] =>
        if mc.shape != this.shape then false
        else if mc.elems == this.elems then true
        else false
      case _: Any => false

  override def toString(): String =
    (for r <- 0 until shape(0)
    yield row(r).mkString("\t")).mkString("[", "\n ", "]")

/** @param values
  * @param rows
  * @param cols
  */
class MatrixNumeric(values: Seq[Double], rows: Int, cols: Int)
    extends Matrix[Double](values.toVector, (rows, cols)):
  override def ++(x: Matrix[Double]): Matrix[Double] =
    require(this.shape == x.shape, "Given matrixes are out of bound in shape!")
    val it = x.elems.iterator
    MatrixNumeric(
      for e <- this.elems yield e + it.next(),
      shape(0),
      shape(1)
    )
  override def --(x: Matrix[Double]): Matrix[Double] =
    require(this.shape == x.shape, "Given matrixes are out of bound in shape!")
    val it = x.elems.iterator
    MatrixNumeric(
      for e <- this.elems yield e - it.next(),
      shape(0),
      shape(1)
    )
  override def **(x: Matrix[Double]): Matrix[Double] =
    require(
      this.shape(1) == x.shape(0),
      "Given matrixes are out of bound in shape!"
    )
    MatrixNumeric(
      for
        r <- 0 until this.shape(0)
        c <- 0 until x.shape(1)
      yield (this.row(r).zip(x.col(c)).map((x, y) => x * y)).sum,
      shape(0),
      x.shape(1)
    )
  override def transpose: Matrix[Double] =
    MatrixNumeric(
      for
        c <- 0 until shape(1)
        r <- 0 until shape(0)
      yield col(c)(r),
      shape(1),
      shape(0)
    )

  override def +(x: Double): Matrix[Double] =
    MatrixNumeric(for e <- elems yield e + x, shape(0), shape(1))
  override def -(x: Double): Matrix[Double] =
    MatrixNumeric(for e <- elems yield e - x, shape(0), shape(1))
  override def *(x: Double): Matrix[Double] =
    MatrixNumeric(for e <- elems yield e * x, shape(0), shape(1))
  override def /(x: Double): Matrix[Double] =
    MatrixNumeric(for e <- elems yield e / x, shape(0), shape(1))

  override def rowsExchanged(i: Int, j: Int): Matrix[Double] =
    require(
      i >= 0 && i < shape(0) && j >= 0 && j < shape(0),
      "Index out of bound!"
    )
    MatrixNumeric(
      (for r <- 0 until shape(0)
      yield
        if r == i then row(j) else if r == j then row(i) else row(r)).flatten,
      shape(0),
      shape(1)
    )
  override def colsExchanged(i: Int, j: Int): Matrix[Double] =
    require(
      i >= 0 && i < shape(1) && j >= 0 && j < shape(1),
      "Index out of bound!"
    )
    MatrixNumeric(
      (for c <- 0 until shape(1)
      yield
        if c == i then col(j) else if c == j then col(i) else col(c)).flatten,
      shape(0),
      shape(1)
    )
  override def rowInserted(
      r: Seq[Double],
      i: Int = shape(0)
  ): Matrix[Double] =
    require(i >= 0 && i <= shape(0), "Index out of bound!")
    MatrixNumeric(
      (for j <- 0 to shape(0)
      yield
        if j < i then row(j)
        else if j == i then r
        else row(j - 1)).flatten,
      shape(0) + 1,
      shape(1)
    )
  override def colInserted(
      c: Seq[Double],
      i: Int = shape(1)
  ): Matrix[Double] =
    require(i >= 0 && i <= shape(1), "Index out of bound!")
    MatrixNumeric(
      for
        r <- 0 until shape(0)
        j <- 0 to shape(1)
      yield
        if j < i then row(r)(j)
        else if j == i then c(r)
        else row(r)(j - 1),
      shape(0),
      shape(1) + 1
    )

  override def hashCode(): Int = elems.hashCode()

object MatrixNumeric:
  def zeros(rows: Int, cols: Int) =
    MatrixNumeric(Vector.fill(rows * cols)(0), rows, cols)
  def zeros(dimention: Int) =
    MatrixNumeric(Vector.fill(dimention * dimention)(0), dimention, dimention)
  def identity(dimention: Int) =
    MatrixNumeric(
      for
        r <- 0 until dimention
        c <- 0 until dimention
      yield
        if r == c then 1
        else 0,
      dimention,
      dimention
    )
  implicit def double2ComplexMat(dm: Matrix[Double]): Matrix[Complex] =
    MatrixComplex(
      for e <- dm.elems yield e,
      dm.shape(0),
      dm.shape(1)
    )

class MatrixComplex(values: Seq[Complex], rows: Int, cols: Int)
    extends Matrix[Complex](values.toVector, (rows, cols)):
  override def ++(x: Matrix[Complex]): Matrix[Complex] =
    require(this.shape == x.shape, "Given matrixes are out of bound in shape!")
    val it = x.elems.iterator
    MatrixComplex(
      for e <- this.elems yield e + it.next(),
      shape(0),
      shape(1)
    )
  override def --(x: Matrix[Complex]): Matrix[Complex] =
    require(this.shape == x.shape, "Given matrixes are out of bound in shape!")
    val it = x.elems.iterator
    MatrixComplex(
      for e <- this.elems yield e - it.next(),
      shape(0),
      shape(1)
    )
  override def **(x: Matrix[Complex]): Matrix[Complex] =
    require(
      this.shape(1) == x.shape(0),
      "Given matrixes are out of bound in shape!"
    )
    MatrixComplex(
      for
        r <- 0 until this.shape(0)
        c <- 0 until x.shape(1)
      yield
        var foo: Complex = 0
        for x <- this.row(r).zip(x.col(c)).map((a, b) => a * b) do foo += x
        foo
      ,
      shape(0),
      x.shape(1)
    )
  override def transpose: Matrix[Complex] =
    MatrixComplex(
      for
        c <- 0 until shape(1)
        r <- 0 until shape(0)
      yield col(c)(r),
      shape(1),
      shape(0)
    )

  override def +(x: Complex): Matrix[Complex] =
    MatrixComplex(for e <- elems yield e + x, shape(0), shape(1))
  override def -(x: Complex): Matrix[Complex] =
    MatrixComplex(for e <- elems yield e - x, shape(0), shape(1))
  override def *(x: Complex): Matrix[Complex] =
    MatrixComplex(for e <- elems yield e * x, shape(0), shape(1))
  override def /(x: Complex): Matrix[Complex] =
    MatrixComplex(for e <- elems yield e / x, shape(0), shape(1))

  override def rowsExchanged(i: Int, j: Int): Matrix[Complex] =
    MatrixComplex(
      (for r <- 0 until shape(0)
      yield
        if r == i then row(j)
        else if r == j then row(i)
        else row(r)).flatten,
      shape(0),
      shape(1)
    )
  override def colsExchanged(i: Int, j: Int): Matrix[Complex] =
    MatrixComplex(
      for
        r <- 0 until shape(0)
        c <- 0 until shape(1)
      yield
        if c == i then row(r)(j)
        else if c == j then row(r)(i)
        else row(r)(c),
      shape(0),
      shape(1)
    )
  override def rowInserted(
      r: Seq[Complex],
      i: Int = shape(0)
  ): Matrix[Complex] =
    require(i >= 0 && i <= shape(0))
    MatrixComplex(
      for
        x <- 0 to shape(0)
        c <- 0 until shape(1)
      yield
        if x < i then row(x)(c)
        else if x == i then r(c)
        else row(x - 1)(c),
      shape(0) + 1,
      shape(1)
    )
  override def colInserted(
      c: Seq[Complex],
      i: Int = shape(1)
  ): Matrix[Complex] =
    require(i >= 0 && i <= shape(1))
    MatrixComplex(
      for
        r <- 0 until shape(0)
        x <- 0 to shape(1)
      yield
        if x < i then row(r)(x)
        else if x == i then c(r)
        else row(r)(x - 1),
      shape(0),
      shape(1) + 1
    )

  override def hashCode(): Int = this.elems.hashCode()

object MatrixComplex:
  def zeros(rows: Int, cols: Int): Matrix[Complex] =
    MatrixComplex(Vector.fill(rows * cols)(Complex.zero), rows, cols)
  def zeros(dimention: Int): Matrix[Complex] =
    MatrixComplex(
      Vector.fill(dimention * dimention)(Complex.zero),
      dimention,
      dimention
    )
  def identity(dimention: Int): Matrix[Complex] =
    MatrixComplex(
      for
        r <- 0 until dimention
        c <- 0 until dimention
      yield
        if r == c then 1
        else 0,
      dimention,
      dimention
    )
