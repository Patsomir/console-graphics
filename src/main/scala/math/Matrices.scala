package math

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
