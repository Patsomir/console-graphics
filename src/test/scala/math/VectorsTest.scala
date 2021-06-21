package math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.AlmostEq.ops._
import utils.Precision
import math.MetricVectorSpace.ops._

class Vector3Test extends AnyFlatSpec with Matchers {
  implicit val precision: Precision = Precision(0.0001)

  "+" should "add two vectors componentwise" in {
    Vector3(1, 2, 3) + Vector3(3, 2, 1) =~ Vector3(4, 4, 4) shouldBe true
    Vector3(1, 2, 5) + Vector3(-1, 2, 0) =~ Vector3(0, 4, 5) shouldBe true
    Vector3(0, 0, 0) + Vector3(-1, 2, -3) =~ Vector3(-1, 2, -3) shouldBe true
    Vector3(0.5f, 0.25f, 1.5f) + Vector3(-1, 2, -3) =~ Vector3(-0.5f, 2.25f, -1.5f) shouldBe true
  }

  "*" should "scale each component of a vector" in {
    Vector3(1, 2, 3) * 2 =~ Vector3(2, 4, 6) shouldBe true
    Vector3(1, 2, 3) * 0.5f =~ Vector3(0.5f, 1, 1.5f) shouldBe true
    Vector3(2, -1, 0.5f) * -4 =~ Vector3(-8, 4, -2) shouldBe true
    Vector3(1, 2, 3) * 0 =~ Vector3(0, 0, 0) shouldBe true
  }

  "dot" should "calculate the dot product of two vectors" in {
    (Vector3(-1, 2, -3) dot Vector3(3, 2, 1)) =~ -2 shouldBe true
    (Vector3(0, 0, 0) dot Vector3(-1, 2, -3)) =~ 0 shouldBe true
    (Vector3(2, 1, 5) dot Vector3(-1, 2, 0)) =~ 0 shouldBe true
    (Vector3(1, 2, 3) dot Vector3(1, 2, 3)) =~ 14 shouldBe true
  }

  "zero" should "return a vector with zero components" in {
    Vector3.zero =~ Vector3(0, 0, 0) shouldBe true
  }

  "vectorProduct" should "return a vector perpendicular to its arguments" in {
    val a = Vector3(1, 2, 5)
    val b = Vector3(-1, 2, 0)
    val c = a vectorProduct b
    (a dot c) =~ 0 shouldBe true
    (b dot c) =~ 0 shouldBe true
  }

  it should "return a zero vector on vectors with the same direction" in {
    val a = Vector3(1, 2, 5)
    val b = a * 2
    (a vectorProduct b) =~ Vector3(0, 0, 0) shouldBe true
  }
}

class Vector4Test extends AnyFlatSpec with Matchers {
  implicit val precision: Precision = Precision(0.0001)

  "+" should "add two vectors componentwise" in {
    Vector4(1, 2, 3, 4) + Vector4(3, 2, 1, 0) =~ Vector4(4, 4, 4, 4) shouldBe true
    Vector4(1, 2, 5, 7) + Vector4(-1, 2, 0, -3) =~ Vector4(0, 4, 5, 4) shouldBe true
    Vector4(0, 0, 0, 0) + Vector4(-1, 2, -3, 4) =~ Vector4(-1, 2, -3, 4) shouldBe true
    Vector4(0.5f, 0.25f, 1.5f, 0.1f) + Vector4(-1, 2, -3, 0.2f) =~ Vector4(-0.5f, 2.25f, -1.5f, 0.3f) shouldBe true
  }

  "*" should "scale each component of a vector" in {
    Vector4(1, 2, 3, 4) * 2 =~ Vector4(2, 4, 6, 8) shouldBe true
    Vector4(1, 2, 3, 4) * 0.5f =~ Vector4(0.5f, 1, 1.5f, 2) shouldBe true
    Vector4(2, -1, 0.5f, -0.25f) * -4 =~ Vector4(-8, 4, -2, 1) shouldBe true
    Vector4(1, 2, 3, 4) * 0 =~ Vector4(0, 0, 0, 0) shouldBe true
  }

  "dot" should "calculate the dot product of two vectors" in {
    (Vector4(-1, 2, -3, 4) dot Vector4(3, 2, 1, 0)) =~ -2 shouldBe true
    (Vector4(0, 0, 0, 0) dot Vector4(-1, 2, -3, 4)) =~ 0 shouldBe true
    (Vector4(2, 1, 5, -2) dot Vector4(-1, 4, 0, 1)) =~ 0 shouldBe true
    (Vector4(1, 2, 3, 4) dot Vector4(1, 2, 3, 4)) =~ 30 shouldBe true
  }

  "zero" should "return a vector with zero components" in {
    Vector4.zero =~ Vector4(0, 0, 0, 0) shouldBe true
  }
}
