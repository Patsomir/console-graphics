package math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Precision
import utils.AlmostEq.ops._
import math.MetricVectorSpace.ops._

class MetricVectorSpaceTest extends AnyFlatSpec with Matchers {
  implicit val precision: Precision = Precision(0.0001)
  implicit val mvs = MetricVectorSpace[Vector3]

  "+" should "be commutative" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    a + b =~ b + a shouldBe true
  }

  it should "be associative" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    val c = Vector3(0, 3, 4)
    (a + b) + c =~ a + (b + c) shouldBe true
  }

  it should "be distriburtive with *" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    (a + b) * 2 =~ a * 2 + b * 2 shouldBe true
  }

  "invert" should "return a vector whose sum with the original vector is zero" in {
    val v = Vector3(1, 2, 3)
    v + v.invert =~ mvs.zero shouldBe true
    v.invert + v =~ mvs.zero shouldBe true
  }

  it should "return a vector equal to the original vector times -1" in {
    val v = Vector3(1, 2, 3)
    v.invert =~ v * -1 shouldBe true
  }

  it should "return the original vector when applied twice" in {
    val v = Vector3(1, 2, 3)
    v.invert.invert =~ v shouldBe true
  }

  "-" should "sum a vector with the invert of another vector" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    a - b =~ a + b.invert shouldBe true
    a - b =~ a + b * -1 shouldBe true
    mvs.zero - b =~ b.invert shouldBe true
  }

  "-" should "return zero when applied to equal arguments" in {
    val v = Vector3(1, 2, 3)
    v - v =~ mvs.zero shouldBe true
  }

  "to" should "return a vector its first argument to its second" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    a + (a to b) =~ b shouldBe true
  }

  "zero" should "be neutral when + is applied" in {
    val v = Vector3(1, 2, 3)
    mvs.zero + v =~ v shouldBe true
    v + mvs.zero =~ v shouldBe true
  }

  "1" should "be neutral when * is applied" in {
    val v = Vector3(1, 2, 3)
    v * 1 =~ v shouldBe true
  }

  it should "return zero when scaled" in {
    mvs.zero * 2 =~ mvs.zero shouldBe true
    mvs.zero * -1 =~ mvs.zero shouldBe true
    mvs.zero * 0 =~ mvs.zero shouldBe true
  }

  "length" should "be positive on a non-zero vector" in {
    Vector3(1, 2, 3).length > 0 shouldBe true
    Vector3(-1, 0, 2).length > 0 shouldBe true
  }

  it should "be zero on a zero vector" in {
    mvs.zero.length =~ 0 shouldBe true
  }

  it should "scale when a vector is scaled" in {
    val v = Vector3(1, 2, 3)
    (v * 2).length =~ v.length * 2 shouldBe true
    (v * -3).length =~ v.length * 3 shouldBe true
    (v * 0).length =~ v.length * 0 shouldBe true
  }

  it should "not violate the triangle rule" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    (a + b).length < a.length + b.length shouldBe true
  }

  "normalize" should "return a vector with length 1 when applied on a non-zero vector" in {
    Vector3(1, 2, 3).normalize.length =~ 1 shouldBe true
    Vector3(-1, 0, 2).normalize.length =~ 1 shouldBe true
    Vector3(0.1f, 0.2f, 0.3f).normalize.length =~ 1 shouldBe true
  }

  it should "return a vector with the same direction" in {
    val v = Vector3(8, 6, 0)
    v.normalize =~ v * 0.1f shouldBe true
  }
}