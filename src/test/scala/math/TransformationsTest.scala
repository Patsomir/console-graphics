package math

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Transformations.LinearTransformation
import math.MetricVectorSpace.ops._
import utils.AlmostEq.ops._
import utils.Precision

class TransformationsTest extends AnyFlatSpec with Matchers {
  implicit val precision: Precision = Precision(0.0001)

  "a perspective matrix" should "project points with z between zero and minus the near plane to points with z less than or equal to -1" in {
    val m = Transformations.perspectiveMatrix(60, 1, 5, 40000)
    m(Vector3(0, 0, -4.9f)).z <= -1 shouldBe true
    m(Vector3(-1, 4, -2)).z <= -1 shouldBe true
    m(Vector3(10, 14, -0.05f)).z <= -1 shouldBe true
  }

  it should "project points with positive z to points with z more than or equal to 1" in {
    val m = Transformations.perspectiveMatrix(60, 1, 5, 40000)
    m(Vector3(0, 0, 1000)).z >= 1 shouldBe true
    m(Vector3(-1, 4, 2)).z >= 1 shouldBe true
    m(Vector3(10, 14, 0.05f)).z >= 1 shouldBe true
  }

  it should "project points with z less than minus the far plane to points with z more than or equal to 1" in {
    val m = Transformations.perspectiveMatrix(60, 1, 5, 40000)
    m(Vector3(0, 0, -100000)).z > 1 shouldBe true
    m(Vector3(1, 4, -40005f)).z >= 1 shouldBe true
  }

  it should "project points with z between minus the near plane and minus the far plane to points with z between -1 and 1" in {
    val m = Transformations.perspectiveMatrix(60, 1, 5, 40000)
    m(Vector3(0, 0, -5.1f)).z >= -1 && m(Vector3(0, 0, -5.1f)).z <= 1 shouldBe true
    m(Vector3(1, 4, -39512f)).z >= -1 && m(Vector3(1, 4, -39512f)).z <= 1 shouldBe true
  }

  "a view matrix" should "transform eye into the zero vector" in {
    val eye = Vector3(1, 2, 3)
    val m = Transformations.viewMatrix(eye, Vector3(-2, 5, 4), Vector3(0, 1, 0))
    m(eye) =~ Vector3.zero shouldBe true
  }

  "a translation matrix" should "add its argument when applied on a vector" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 5)
    
    val t1 = Vector3(1, 0, 0)
    val t2 = Vector3(2, 3, 4)

    Transformations.translationMatrix(t1)(a) =~ a + t1 shouldBe true
    Transformations.translationMatrix(t1)(b) =~ b + t1 shouldBe true
    Transformations.translationMatrix(t2)(a) =~ a + t2 shouldBe true
    Transformations.translationMatrix(t2)(b) =~ b + t2 shouldBe true
  }

  it should "work as identity with a zero vector argument" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 5)
    val t = Transformations.translationMatrix(Vector3.zero)

    t(a) =~ a shouldBe true
    t(b) =~ b shouldBe true
  }

  "a scaling matrix" should "scale a vector's components with the corresponding scalers" in {
    val a = Vector3(1, 2, 3)
    val s = Transformations.scalingMatrix(2, 0, -1)
    
    s(a) =~ Vector3(a.x * 2, a.y * 0, a.z * -1) shouldBe true
  }

  it should "result in a zero vector when using only 0 scalers" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 5)
    val s = Transformations.scalingMatrix(0, 0, 0)

    s(a) =~ Vector3.zero shouldBe true
    s(b) =~ Vector3.zero shouldBe true
  }

  it should "work as identity when using only 1 scalers" in {
    val a = Vector3(1, 2, 3)
    val b = Vector3(-1, 0, 5)
    val s = Transformations.scalingMatrix(1, 1, 1)

    s(a) =~ a shouldBe true
    s(b) =~ b shouldBe true
  }

  "a x rotation matrix" should "rotate the unit axis vectors around the x axis by the given angle in degrees" in {
    val x = Vector3(1, 0, 0)
    val y = Vector3(0, 1, 0)
    val z = Vector3(0, 0, 1)
    val r1 = Transformations.xRotationMatrix(90)
    val r2 = Transformations.xRotationMatrix(-90)

    r1(x) =~ x shouldBe true
    r1(y) =~ z shouldBe true
    r1(z) =~ y.invert shouldBe true

    r2(x) =~ x shouldBe true
    r2(y) =~ z.invert shouldBe true
    r2(z) =~ y shouldBe true
  }

  it should "work as identity with a zero degrees angle" in {
    val a = Vector3(1, 2, 3)
    val r = Transformations.xRotationMatrix(0)

    r(a) =~ a shouldBe true
  }

  "a y rotation matrix" should "rotate the unit axis vectors around the y axis by the given angle in degrees" in {
    val x = Vector3(1, 0, 0)
    val y = Vector3(0, 1, 0)
    val z = Vector3(0, 0, 1)
    val r1 = Transformations.yRotationMatrix(90)
    val r2 = Transformations.yRotationMatrix(-90)

    r1(x) =~ z.invert shouldBe true
    r1(y) =~ y shouldBe true
    r1(z) =~ x shouldBe true

    r2(x) =~ z shouldBe true
    r2(y) =~ y shouldBe true
    r2(z) =~ x.invert shouldBe true
  }

  it should "work as identity with a zero degrees angle" in {
    val a = Vector3(1, 2, 3)
    val r = Transformations.yRotationMatrix(0)

    r(a) =~ a shouldBe true
  }

  "a z rotation matrix" should "rotate the unit axis vectors around the z axis by the given angle in degrees" in {
    val x = Vector3(1, 0, 0)
    val y = Vector3(0, 1, 0)
    val z = Vector3(0, 0, 1)
    val r1 = Transformations.zRotationMatrix(90)
    val r2 = Transformations.zRotationMatrix(-90)

    r1(x) =~ y shouldBe true
    r1(y) =~ x.invert shouldBe true
    r1(z) =~ z shouldBe true

    r2(x) =~ y.invert shouldBe true
    r2(y) =~ x shouldBe true
    r2(z) =~ z shouldBe true
  }

  it should "work as identity with a zero degrees angle" in {
    val a = Vector3(1, 2, 3)
    val r = Transformations.zRotationMatrix(0)

    r(a) =~ a shouldBe true
  }
}