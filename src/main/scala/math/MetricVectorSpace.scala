package math

import scala.math.sqrt

trait MetricVectorSpace[V] {
  def add(left: V, right: V): V
  def scale(vec: V, scaler: Float): V
  def dot(left: V, right: V): Float
  def zero: V

  def invert(vec: V): V = scale(vec, -1)
  def sub(left: V, right: V): V = add(left, invert(right))
  def to(left: V, right: V): V = sub(right, left)
  def length(vec: V): Float = sqrt(dot(vec, vec)).toFloat
  def normalize(vec: V): V = scale(vec, 1 / length(vec))
  def distance(left: V, right: V) = length(to(left, right))
}

object MetricVectorSpace {
  def apply[V](implicit ms: MetricVectorSpace[V]): MetricVectorSpace[V] = ms

  object ops {
    implicit class MetricVector[V](vec: V)(implicit ms: MetricVectorSpace[V]) {
      def +(other: V): V = ms.add(vec, other)
      def *(scaler: Float): V = ms.scale(vec, scaler)
      def dot(other: V): Float = ms.dot(vec, other)

      def invert: V = ms.invert(vec)
      def -(other: V): V = ms.sub(vec, other)
      def to(other: V): V = ms.to(vec, other)
      def length: Float = ms.length(vec)
      def normalize: V = ms.normalize(vec)
      def distance(other: V) = ms.distance(vec, other)
    }
  }
}