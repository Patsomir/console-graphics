package math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.Precision
import utils.AlmostEq.ops._
import math.MetricSpace.ops._

class MetricSpaceTest extends AnyFlatSpec with Matchers {
  implicit val precision: Precision = Precision(0.0001)
  implicit val ms = MetricVectorSpace[Vector3]

  "distance" should "be positive when applied on different vectors" in {
    (Vector3(1, 2, 3) distanceTo Vector3(1, 2, 5)) > 0 shouldBe true
    (Vector3(1, 2, 3) distanceTo Vector3(-1, 0, 2)) > 0 shouldBe true
    (Vector3(-1, 0, 2) distanceTo Vector3(0, 0, 0)) > 0 shouldBe true
  }

  it should "be zero on equal vectors" in {
    (Vector3(1, 2, 3) distanceTo Vector3(1, 2, 3)) =~ 0 shouldBe true
    (Vector3(0, 0, 0) distanceTo Vector3(0, 0, 0)) =~ 0 shouldBe true
  }

  it should "be comutative" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    (a distanceTo b) =~ (b distanceTo a) shouldBe true
  }

  it should "not violate the triangle property" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 2)
    val c = Vector3(0, 3, 4)
    (a distanceTo c) < (a distanceTo b) + (b distanceTo c) shouldBe true
    (b distanceTo a) < (b distanceTo c) + (c distanceTo a) shouldBe true
    (c distanceTo b) < (c distanceTo a) + (a distanceTo b) shouldBe true
  }
}