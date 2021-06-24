package app

import console.AnsiString
import graphics.Color
import graphics.Canvas
import graphics.Point
import math.Vector3
import graphics.meshes.Cube
import graphics.MatrixTransformer
import graphics.PerspectiveProjector
import console.AnsiConsumer
import math.MetricVectorSpace.ops._
import math.Transformations.LinearTransformation
import scala.math.sin
import cats.effect.IO
import cats.effect.IOApp
import scala.concurrent.duration._

object AnimationApp extends IOApp.Simple {
  val width = 120
  val height = 40

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
  val cube = Cube(1f)
 
  val canvas = Canvas(width, height)
  val transformer = MatrixTransformer(Vector3(1, 0.5f, 0), 0.5f, 0.5f, 1.5f, 45, -45, 30)

  val amp = 5.0f
  val speed = 0.15f

  def scene(time: Float): String = {
    val x = (sin(time * speed) * amp).toFloat
    val eye = Vector3(x, 2, 4)
    val focus = Vector3(0, 0, 0)
    val up = Vector3(0, 1, 0)

    val projector = PerspectiveProjector(eye, focus, up, eye to focus, 0.5f * width.toFloat / height)
    val primitives = axis.map { case (a, b, color) => projector(a, b, color) } ++ projector(
      transformer(cube),
      Color(true, true, true, 1)
    )
    AnsiConsumer(canvas(primitives)).toString
  }

  def display(time: Float): IO[Unit] = for {
    _ <- IO.println(scene(time))
    _ <- IO.sleep(37.millis)
  } yield ()
  
  def run: IO[Unit] = Range(1, 120).foldLeft(IO.unit)((acc, i) => acc *> display(i))
}
