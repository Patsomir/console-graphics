package math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Precision
import utils.AlmostEq.ops._
import math.Matrix4x4.tupleToVector4

class Matrix4x4Test extends AnyFlatSpec with Matchers {
  implicit val precision: Precision = Precision(0.0001)

  "transpose" should "transpose a matrix" in {
    val a = (1f, 2f, 3f, 4f)
    val b = (-1f, 3f, 2f, 0f)
    val c = (1f, 12f, 2f, 4f)
    val d = (11f, 2f, 12f, -4f)
    val m = Matrix4x4.fromCols(a, b, c, d)

    m.transpose =~ Matrix4x4.fromRows(a, b, c, d) shouldBe true
    m.transpose =~ Matrix4x4.fromCols(
      (1f, -1f, 1f, 11f),
      (2f, 3f, 12f, 2f),
      (3f, 2f, 2f, 12f),
      (4f, 0f, 4f, -4f)
    ) shouldBe true
    m.transpose.transpose =~ m shouldBe true
  }

  "*" should "multiply a matrix with a vector" in {
    val m = Matrix4x4.fromRows(
      (1f, 2f, 3f, 1f),
      (0f, 1f, 3f, 0f),
      (0f, 2f, 3f, 3f),
      (0f, -2f, 3f, 1f)
    )
    m * (0f, 1f, 0f, 0f) =~ (2f, 1f, 2f, -2f) shouldBe true
    m * (0f, 0f, 0f, 0f) =~ (0f, 0f, 0f, 0f) shouldBe true
    m * (1f, 2f, -1f, 0f) =~ (2f, -1f, 1f, -7f) shouldBe true
  }

  it should "multiply two matrices" in {
    val m1 = Matrix4x4.fromRows(
      (1f, 2f, 3f, 1f),
      (0f, 1f, 3f, 0f),
      (0f, 2f, 3f, 3f),
      (0f, -2f, 3f, 1f)
    )
    val m2 = Matrix4x4.fromRows(
      (0f, 0f, 0f, 1f),
      (1f, 0f, 2f, 1f),
      (0f, 0f, -1f, 0f),
      (0f, 0f, 0f, 0f)
    )
    val m3 = Matrix4x4.fromRows(
      (2f, 0f, 1f, 3f),
      (1f, 0f, -1f, 1f),
      (2f, 0f, 1f, 2f),
      (-2f, 0f, -7f, -2f)
    )
    m1 * m2 =~ m3 shouldBe true
  }
}
