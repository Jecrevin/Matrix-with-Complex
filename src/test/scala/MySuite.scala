import jecrevin.math.Complex
import jecrevin.math.MatrixNumeric
// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {
  val c1 = Complex(1, 3)
  val c2 = Complex(3, 5)

  test("Times for Complex") {
    val obtained = c1 * c2
    val expected = Complex(-12, 14)
    assertEquals(obtained, expected)
  }

  test("Division for Complex") {
    val obtained = c1 / c2
    val expected = Complex(18 / 34d, 4 / 34d)
    assertEquals(obtained, expected)
  }

  val m1 = MatrixNumeric(Vector(1, 2, 3, 4), 2, 2)
  val m2 = MatrixNumeric(Vector(2, 0, 1, 3), 2, 2)
  test("Dot for Matrix") {
    val obtained = m1 ** m2
    val expected = MatrixNumeric(Vector(4, 6, 10, 12), 2, 2)
    assertEquals(obtained,expected)
  }
    
}
