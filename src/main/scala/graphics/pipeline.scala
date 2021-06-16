package graphics

import math.Vector3

trait Transformer {
  import math.MetricVectorSpace.ops._

  def normalTransform(vec: Vector3): Vector3
  def pointTransform(vec: Vector3): Vector3

  def apply(surface: Surface, pivot: Vector3 = Vector3(0, 0, 0)): Surface = Surface(
    pointTransform(surface.a - pivot) + pivot,
    pointTransform(surface.b - pivot) + pivot,
    pointTransform(surface.c - pivot) + pivot,
    normalTransform(surface.normal)
  )

  def apply(mesh: Mesh, pivot: Vector3): Mesh = Mesh.withSurfaces(mesh.surfaces.map(apply(_, pivot)))

  def apply(mesh: Mesh): Mesh = Mesh.withSurfaces(mesh.surfaces.map(apply(_)))
}

case class MatrixTransformer(
  position: Vector3,
  xScale: Float = 1, yScale: Float = 1, zScale: Float = 1,
  xRotation: Float = 0, yRotation: Float = 0, zRotation: Float = 0
) extends Transformer {
  import math.Transformations._

  val normalTransformationMatrix = yRotationMatrix(yRotation) * xRotationMatrix(xRotation) * zRotationMatrix(zRotation) * scalingMatrix(xScale, yScale, zScale)
  val pointTransformationMatrix = translationMatrix(position) * normalTransformationMatrix
  
  override def normalTransform(vec: Vector3): Vector3 = normalTransformationMatrix(vec)
  override def pointTransform(vec: Vector3): Vector3 = pointTransformationMatrix(vec)
}