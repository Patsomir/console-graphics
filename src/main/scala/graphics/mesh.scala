package graphics

import math.Vector3

case class Surface(a: Vector3, b: Vector3, c: Vector3, normal: Vector3)

trait Mesh {
  def surfaces: List[Surface]
}

object Mesh {
  def withSurfaces(list: List[Surface]): Mesh = new Mesh { val surfaces = list }
}
