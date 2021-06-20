package graphics

import math.Vector3

case class PerspectiveProjector(
  eye: Vector3,
  focus: Vector3,
  up: Vector3,
  lightDir: Vector3,
  aspectRatio: Float,
  viewAngle: Float = 30,
  nearPlane: Float = 1,
  farPlane: Float = 40000
) extends OutlineProjector with MeshProjector {
  import math.Transformations._
  import math.MetricVectorSpace.ops._

  val projectionMatrix = perspectiveMatrix(viewAngle, aspectRatio, nearPlane, farPlane) * viewMatrix(eye, focus, up)
  val normalizedLightDir = lightDir.invert.normalize

  def intensityScaler(normal: Vector3): Float = {
    val scale = normal.normalize dot normalizedLightDir
    if (scale > 0) scale
    else 0
  }

  def project(surface: Surface, color: Color): Triangle = Triangle(
    projectionMatrix(surface.a),
    projectionMatrix(surface.b),
    projectionMatrix(surface.c),
    color.copy(intensity = color.intensity * intensityScaler(surface.normal))
  )

  def project(point: Vector3): Point = projectionMatrix(point)
}