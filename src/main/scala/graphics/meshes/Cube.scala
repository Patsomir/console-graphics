package graphics.meshes

import graphics.Mesh
import graphics.Surface
import math.Vector3

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
      Surface(Vector3(-hs, hs, -hs), Vector3(hs, hs, -hs), Vector3(-hs, hs, hs), Vector3(0, 1, 0))
    )
  }
}