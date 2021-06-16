package app

import console.AnsiOps.AnsiString
import graphics.Color
import graphics.Canvas
import graphics.Line
import graphics.Point
import graphics.CanvasFragment
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import graphics.Vertex
import graphics.Triangle
import math.Transformations
import java.{util => ju}
import math.Vector3
import graphics.Cube
import graphics.MatrixTransformer

object Palette {
  private val chars = Array(' ', '`', '.', ':', '-', '~', '=', 'o', '*', '#', '%', '@', '&', '$', 'M', 'W');

  def stringify(color: Color): AnsiString = {
    val Color(r, g, b, intensity) = color
    AnsiString.empty.color(r ,g , b).add(chars((0.5f + intensity * (chars.length - 1)).toInt).toString).reset
  }
}

object Start extends App {
  val width = 120
  val height = 40
  val canvas = Canvas(120, 40)
  val out = new BufferedWriter(new OutputStreamWriter(new
    FileOutputStream(java.io.FileDescriptor.out), "ASCII"), width * height);


  
  val axis = List(
    Line(Point(0, 0, 0), Point(1, 0, 0), Color(true, false, false, 1)),
    Line(Point(1, 0, 0), Point(1, 0, 1), Color(true, false, false, 1)),
    Line(Point(1, 0, 1), Point(0, 0, 1), Color(true, false, false, 1)),
    Line(Point(0, 0, 1), Point(0, 0, 0), Color(true, false, false, 1)),

    Line(Point(0, 1, 0), Point(1, 1, 0), Color(false, true, false, 1)),
    Line(Point(1, 1, 0), Point(1, 1, 1), Color(false, true, false, 1)),
    Line(Point(1, 1, 1), Point(0, 1, 1), Color(false, true, false, 1)),
    Line(Point(0, 1, 1), Point(0, 1, 0), Color(false, true, false, 1)),

    Line(Point(0, 0, 0), Point(0, 1, 0), Color(false, false, true, 1)),
    Line(Point(1, 0, 0), Point(1, 1, 0), Color(false, false, true, 1)),
    Line(Point(1, 0, 1), Point(1, 1, 1), Color(false, false, true, 1)),
    Line(Point(0, 0, 1), Point(0, 1, 1), Color(false, false, true, 1)),

    Line(Point(-5, 0, 0), Point(5, 0, 0), Color(true, true, true, 0.1f)),
    Line(Point(0, -5, 0), Point(0, 5, 0), Color(true, true, true, 0.1f)),
    Line(Point(0, 0, -5), Point(0, 0, 5), Color(true, true, true, 0.1f)),
  )

  Range(0, 3).foreach(j => {

  val steps = 30
  Range(0, steps).foreach(i => {
    val x = -5 + 10 * (((i.toFloat / (steps - 1)) * 2) - 1).abs
    val eye = Vector3(x, 2, 4)
    val focus = Vector3(0, 0, 0)
    val up = Vector3(0, 1, 0)

    import math.MetricVectorSpace.ops._
    import math.Transformations.LinearTransformation

    def lightIntensity(normal: Vector3, dof: Vector3): Float = {
      val scale = normal.normalize dot dof.normalize
      if(scale > 0) scale
      else 0
    }

    val transformer = Transformations.applyMatrix(Transformations.perspectiveMatrix(30, 0.5f * width.toFloat / height, 1, 40000) * Transformations.viewMatrix(eye, focus, up)) _


    val f = MatrixTransformer(Vector3(1, 0.5f, 0), 0.5f, 0.5f, 1.5f, 45, 45, 30)

    val primitives = axis ++ f(Cube(1f)).surfaces.map(s => Triangle(s.a, s.b, s.c, Color(false, true, true, lightIntensity(s.normal, focus to eye))))

    out.write {
      canvas(primitives.map(transformer))
        .foldRows(AnsiString.empty) {
          case (acc, CanvasFragment(color, _)) => acc + Palette.stringify(color)
        }
        .reverse.zipWithIndex.foldLeft(AnsiString.empty.eraseScreen) {
          case (acc, (row, index)) => acc.moveTo(index + 1, 1) + row
        }.toString
    }
    out.flush()

    Thread.sleep(37)
  })

  })
  
}