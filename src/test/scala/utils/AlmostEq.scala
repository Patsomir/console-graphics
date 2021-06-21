package utils

import math.Vector3
import math.Vector4
import math.Matrix4x4

case class Precision(value: Double) {
  require(value > 0)
}

trait AlmostEq[A] {
  def almostEqual(a: A, b: A)(implicit precision: Precision): Boolean
}

object AlmostEq {
  object ops {
    implicit class AlmostEqElem[A](elem: A)(implicit ae: AlmostEq[A]) {
      def =~(other: A)(implicit precision: Precision): Boolean = ae.almostEqual(elem, other)
    }
  }

  import ops._

  implicit val floatAlmostEq: AlmostEq[Float] = new AlmostEq[Float] {
    override def almostEqual(a: Float, b: Float)(implicit precision: Precision): Boolean = (a - b).abs < precision.value
  }

  implicit val vector3AlmostEq: AlmostEq[Vector3] = new AlmostEq[Vector3] {
    override def almostEqual(a: Vector3, b: Vector3)(implicit precision: Precision): Boolean
      = a.x =~ b.x && a.y =~ b.y && a.z =~ b.z
  }

  implicit val vector4AlmostEq: AlmostEq[Vector4] = new AlmostEq[Vector4] {
    override def almostEqual(a: Vector4, b: Vector4)(implicit precision: Precision): Boolean
      = a.x =~ b.x && a.y =~ b.y && a.z =~ b.z && a.t =~ b.t
  }

  implicit val matrix4x4AlmostEq: AlmostEq[Matrix4x4] = new AlmostEq[Matrix4x4] {
    override def almostEqual(a: Matrix4x4, b: Matrix4x4)(implicit precision: Precision): Boolean
      = a.col1 =~ b.col1 && a.col2 =~ b.col2 && a.col3 =~ b.col3 && a.col4 =~ b.col4
  }
}