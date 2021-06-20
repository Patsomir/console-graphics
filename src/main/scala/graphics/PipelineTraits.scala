package graphics

import math.Vector3
import java.awt.PointerInfo
import javax.swing.InputVerifier

trait Transformer {
  import math.MetricVectorSpace.ops._

  def normalTransform(vec: Vector3): Vector3
  def pointTransform(vec: Vector3): Vector3

  def apply(point: Vector3): Vector3 = pointTransform(point)

  def apply(a: Vector3, b: Vector3): (Vector3, Vector3) = (pointTransform(a), pointTransform(b))

  def apply(surface: Surface): Surface = Surface(
    pointTransform(surface.a),
    pointTransform(surface.b),
    pointTransform(surface.c),
    normalTransform(surface.normal)
  )

  def apply(mesh: Mesh): Mesh = Mesh.withSurfaces(mesh.surfaces.map(apply))

  def transformLines(lines: Iterable[(Vector3, Vector3)]): Iterable[(Vector3, Vector3)] = lines.map {
    case (a, b) => apply(a, b)
  }

  def transformPoints(points: Iterable[Vector3]): Iterable[Vector3] = points.map(apply)
}

trait MeshProjector {
  def project(surface: Surface, color: Color): Triangle

  def apply(surface: Surface, color: Color): Triangle = project(surface, color)
  def apply(mesh: Mesh, color: Color): Iterable[Triangle] = mesh.surfaces.map(apply(_, color))
}

trait OutlineProjector {
  def project(point: Vector3): Point

  def apply(a: Vector3, b: Vector3, color: Color): Line = Line(project(a), project(b), color)
  def apply(point: Vector3, color: Color): Vertex = Vertex(point, color)

  def projectLines(lines: Iterable[(Vector3, Vector3)], color: Color): Iterable[Line] = lines.map {
    case (a, b) => apply(a, b, color)
  }

  def projectPoints(points: Iterable[Vector3], color: Color): Iterable[Vertex] = points.map(apply(_, color))
}

trait Rasterizer {
  def rasterize(primitives: Iterable[Primitive]): Raster

  def apply(primitives: Iterable[Primitive]): Raster = rasterize(primitives)
}

case class RasterFragment(color: Color, depth: Float)

trait Raster {
  def foldLeft[A](unit: A)(f: (A, RasterFragment) => A): List[A]
  def foldRight[A](unit: A)(f: (RasterFragment, A) => A): List[A]

  def foldLeftUp[A, B](rowUnit: A, unit: B)(rowFolder: (A, RasterFragment) => A, folder: (B, A) => B): B =
    foldLeft(rowUnit)(rowFolder).foldLeft(unit)(folder)

  def foldLeftDown[A, B](rowUnit: A, unit: B)(rowFolder: (A, RasterFragment) => A, folder: (A, B) => B): B =
    foldLeft(rowUnit)(rowFolder).foldRight(unit)(folder)

  def foldRightUp[A, B](rowUnit: A, unit: B)(rowFolder: (RasterFragment, A) => A, folder: (B, A) => B): B =
    foldRight(rowUnit)(rowFolder).foldLeft(unit)(folder)

  def foldRightDown[A, B](rowUnit: A, unit: B)(rowFolder: (RasterFragment, A) => A, folder: (A, B) => B): B =
    foldRight(rowUnit)(rowFolder).foldRight(unit)(folder)
}
