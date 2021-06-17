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

trait Projector {
  def project(surface: Surface, color: Color): Triangle

  def apply(surface: Surface, color: Color): Triangle = project(surface, color)
  def apply(mesh: Mesh, color: Color): List[Triangle] = mesh.surfaces.map(apply(_, color))
}

trait Rasterizer {
  def rasterize(primitives: List[Primitive]): Raster

  def apply(primitives: List[Primitive]): Raster = rasterize(primitives)
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
