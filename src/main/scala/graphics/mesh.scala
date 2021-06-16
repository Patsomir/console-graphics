package graphics

import math.Vector3

case class Surface(a: Vector3, b: Vector3, c: Vector3, normal: Vector3)

trait Mesh {
  def surfaces: List[Surface]
}

object Mesh {
  def withSurfaces(list: List[Surface]): Mesh = new Mesh { val surfaces = list }
}

case class Cube(side: Float) extends Mesh {

  override val surfaces: List[Surface] = {
    val hs = side / 2
    List(
      Surface(Vector3(hs, -hs, hs), Vector3(hs, -hs, -hs), Vector3(-hs, -hs, hs), Vector3(0, -1, 0)),
      Surface(Vector3(-hs, -hs, -hs), Vector3(hs, -hs, -hs), Vector3(-hs, -hs, hs), Vector3(0, -1, 0)),

      Surface(Vector3(hs, -hs, hs), Vector3(hs, -hs, -hs), Vector3(hs, hs, -hs), Vector3(1, 0, 0)),
      Surface(Vector3(hs, -hs, hs), Vector3(hs, hs, hs), Vector3(hs, hs, -hs), Vector3(1, 0, 0)),

      Surface(Vector3(hs, -hs, hs), Vector3(hs, hs, hs), Vector3(-hs, -hs, hs), Vector3(0, 0, 1)),
      Surface(Vector3(-hs, hs, hs), Vector3(hs, hs, hs), Vector3(-hs, -hs, hs), Vector3(0, 0, 1)),

      Surface(Vector3(-hs, hs, hs), Vector3(-hs, -hs, -hs), Vector3(-hs, -hs, hs), Vector3(-1, 0, 0)),
      Surface(Vector3(-hs, hs, hs), Vector3(-hs, -hs, -hs), Vector3(-hs, hs, -hs), Vector3(-1, 0, 0)),

      Surface(Vector3(hs, -hs, -hs), Vector3(-hs, -hs, -hs), Vector3(-hs, hs, -hs), Vector3(0, 0, -1)),
      Surface(Vector3(hs, -hs, -hs), Vector3(hs, hs, -hs), Vector3(-hs, hs, -hs), Vector3(0, 0, -1)),
      
      Surface(Vector3(hs, hs, hs), Vector3(hs, hs, -hs), Vector3(-hs, hs, hs), Vector3(0, 1, 0)),
      Surface(Vector3(-hs, hs, -hs), Vector3(hs, hs, -hs), Vector3(-hs, hs, hs), Vector3(0, 1, 0)),
    )
  }

}