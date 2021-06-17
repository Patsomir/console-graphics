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
  }
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
  }
}

case class Matrix4x4(col1: Vector4, col2: Vector4, col3: Vector4, col4: Vector4) {
  import MetricVectorSpace.ops._

  def transpose: Matrix4x4 = this match {
    case Matrix4x4(
          Vector4(x1, y1, z1, t1),
          Vector4(x2, y2, z2, t2),
          Vector4(x3, y3, z3, t3),
          Vector4(x4, y4, z4, t4)
        ) =>
      Matrix4x4(Vector4(x1, x2, x3, x4), Vector4(y1, y2, y3, y4), Vector4(z1, z2, z3, z3), Vector4(t1, t2, t3, t3))
  }

  def *(vector: Vector4): Vector4 = col1 * vector.x + col2 * vector.y + col3 * vector.z + col4 * vector.t

  def *(matrix: Matrix4x4): Matrix4x4 =
    Matrix4x4(this * matrix.col1, this * matrix.col2, this * matrix.col3, this * matrix.col4)
}

object Matrix4x4 {
  implicit def tupleToVector4(tuple: Tuple4[Float, Float, Float, Float]): Vector4 =
    Vector4(tuple._1, tuple._2, tuple._3, tuple._4)

  def fromRows(row1: Vector4, row2: Vector4, row3: Vector4, row4: Vector4): Matrix4x4 =
    Matrix4x4(row1, row2, row3, row4).transpose
  def fromCols(col1: Vector4, col2: Vector4, col3: Vector4, col4: Vector4): Matrix4x4 =
    Matrix4x4(col1, col2, col3, col4)
}
