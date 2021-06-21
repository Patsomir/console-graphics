package math

case class Vector4(x: Float, y: Float, z: Float, t: Float)

object Vector4 {
  implicit val metricSpaceVector3: MetricVectorSpace[Vector4] = new MetricVectorSpace[Vector4] {
    override def add(left: Vector4, right: Vector4): Vector4 =
      Vector4(left.x + right.x, left.y + right.y, left.z + right.z, left.t + right.t)
    override def scale(vec: Vector4, scaler: Float): Vector4 =
      Vector4(vec.x * scaler, vec.y * scaler, vec.z * scaler, vec.t * scaler)
    override def dot(left: Vector4, right: Vector4): Float =
      left.x * right.x + left.y * right.y + left.z * right.z + left.t * right.t
    override def zero: Vector4 = Vector4(0, 0, 0, 0)
  }

  val zero = MetricVectorSpace[Vector4].zero
}

case class Vector3(x: Float, y: Float, z: Float) {
  def vectorProduct(other: Vector3): Vector3 =
    Vector3(y * other.z - z * other.y, z * other.x - x * other.z, x * other.y - y * other.x)
}

object Vector3 {
  implicit val metricSpaceVector3: MetricVectorSpace[Vector3] = new MetricVectorSpace[Vector3] {
    override def add(left: Vector3, right: Vector3): Vector3 =
      Vector3(left.x + right.x, left.y + right.y, left.z + right.z)
    override def scale(vec: Vector3, scaler: Float): Vector3 =
      Vector3(vec.x * scaler, vec.y * scaler, vec.z * scaler)
    override def dot(left: Vector3, right: Vector3): Float =
      left.x * right.x + left.y * right.y + left.z * right.z
    override def zero: Vector3 = Vector3(0, 0, 0)
  }

  val zero = MetricVectorSpace[Vector3].zero
}
