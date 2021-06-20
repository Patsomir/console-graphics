package graphics

import math.Vector3

case class MatrixTransformer(
  position: Vector3,
  xScale: Float = 1,
  yScale: Float = 1,
  zScale: Float = 1,
  xRotation: Float = 0,
  yRotation: Float = 0,
  zRotation: Float = 0
) extends Transformer {
  import math.Transformations._

  val normalTransformationMatrix =
    yRotationMatrix(yRotation) * xRotationMatrix(xRotation) * zRotationMatrix(zRotation) * scalingMatrix(
      xScale,
      yScale,
      zScale
    )
  val pointTransformationMatrix = translationMatrix(position) * normalTransformationMatrix

  override def normalTransform(vec: Vector3): Vector3 = normalTransformationMatrix(vec)
  override def pointTransform(vec: Vector3): Vector3 = pointTransformationMatrix(vec)

  def transformLines(lines: List[(Vector3, Vector3)]): List[(Vector3, Vector3)] = lines.map {
    case (a, b) => (pointTransform(a), pointTransform(b))
  }
}