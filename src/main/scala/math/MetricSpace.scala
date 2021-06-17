package math

trait MetricSpace[A] {
  def distance(left: A, right: A): Float
}

object MetricSpace {
  def apply[A](implicit ms: MetricSpace[A]): MetricSpace[A] = ms

  object ops {
    implicit class MetricElement[A](elem: A)(implicit ms: MetricSpace[A]) {
      def distanceTo(other: A) = ms.distance(elem, other)
    }
  }
}