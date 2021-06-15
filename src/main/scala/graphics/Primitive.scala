package graphics

import math.Vector3

case class Point(x: Float, y: Float, z: Float)

object Point {
  implicit def pointToVector3(point: Point): Vector3 = Vector3(point.x, point.y, point.z)
  implicit def vector3ToPoint(vec: Vector3): Point = Point(vec.x, vec.y, vec.z)
}

case class Color(r: Boolean, g: Boolean, b: Boolean, intensity: Float)

sealed trait Primitive {
  def color: Color
}

case class Vertex(position: Point, color: Color) extends Primitive
case class Line(start: Point, end: Point, color: Color) extends Primitive
case class Triangle(a: Point, b: Point, c: Point, color: Color) extends Primitive
