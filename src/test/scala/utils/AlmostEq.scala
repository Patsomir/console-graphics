package utils

case class Precision(value: Double)

trait AlmostEq[A] {
  def almostEqual(a: A, b: A)(implicit precision: Precision): Boolean
}

object AlmostEq {
  object ops {
    implicit class AlmostEqElem[A](elem: A)(implicit ae: AlmostEq[A]) {
      def =~(other: A)(implicit precision: Precision): Boolean = ae.almostEqual(elem, other)
    }
  }

  implicit val floatAlmostEq: AlmostEq[Float] = new AlmostEq[Float] {
    override def almostEqual(a: Float, b: Float)(implicit precision: Precision): Boolean = (a - b).abs < precision.value
  }
}