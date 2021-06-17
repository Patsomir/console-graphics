package app

import console.AnsiString
import graphics.Color
import graphics.Canvas
import graphics.Point
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import math.Vector3
import graphics.meshes.Cube
import graphics.MatrixTransformer
import graphics.PerspectiveProjector
import console.AnsiConsumer
import math.MetricVectorSpace.ops._
import math.Transformations.LinearTransformation

object Start extends App {
  val width = 120
  val height = 40
  val canvas = Canvas(120, 40)
  val out = new BufferedWriter(
    new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"),
    width * height
  );

  val axis = List(
    (Point(0, 0, 0), Point(1, 0, 0), Color(true, false, false, 1)),
    (Point(1, 0, 0), Point(1, 0, 1), Color(true, false, false, 1)),
    (Point(1, 0, 1), Point(0, 0, 1), Color(true, false, false, 1)),
    (Point(0, 0, 1), Point(0, 0, 0), Color(true, false, false, 1)),
    (Point(0, 1, 0), Point(1, 1, 0), Color(false, true, false, 1)),
    (Point(1, 1, 0), Point(1, 1, 1), Color(false, true, false, 1)),
    (Point(1, 1, 1), Point(0, 1, 1), Color(false, true, false, 1)),
    (Point(0, 1, 1), Point(0, 1, 0), Color(false, true, false, 1)),
    (Point(0, 0, 0), Point(0, 1, 0), Color(false, false, true, 1)),
    (Point(1, 0, 0), Point(1, 1, 0), Color(false, false, true, 1)),
    (Point(1, 0, 1), Point(1, 1, 1), Color(false, false, true, 1)),
    (Point(0, 0, 1), Point(0, 1, 1), Color(false, false, true, 1)),
    (Point(-5, 0, 0), Point(5, 0, 0), Color(true, true, true, 0.1f)),
    (Point(0, -5, 0), Point(0, 5, 0), Color(true, true, true, 0.1f)),
    (Point(0, 0, -5), Point(0, 0, 5), Color(true, true, true, 0.1f))
  )

  Range(0, 3).foreach(j => {
    val steps = 30
    Range(0, steps).foreach(i => {
      val x = -5 + 10 * (((i.toFloat / (steps - 1)) * 2) - 1).abs
      val eye = Vector3(x, 2, 4)
      val focus = Vector3(0, 0, 0)
      val up = Vector3(0, 1, 0)

      val projector = PerspectiveProjector(eye, focus, up, eye to focus, 0.5f * width.toFloat / height)
      val transformer = MatrixTransformer(Vector3(1, 0.5f, 0), 0.5f, 0.5f, 1.5f, 45, 45, 30)
      val primitives = axis.map { case (a, b, color) => projector.project(a, b, color) } ++ projector(
        transformer(Cube(1f)),
        Color(true, true, true, 1)
      )
      val ansi = AnsiConsumer(canvas(primitives)).toString

      out.write(ansi)
      out.flush()

      Thread.sleep(37)
    })
  })
}
