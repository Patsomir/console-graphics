package app.tetris

import graphics.Canvas
import scala.math.{ cos, sin, toRadians }
import math.Vector3
import graphics.Color
import graphics.PerspectiveProjector
import TetrisDrawingTools._
import math.MetricVectorSpace.ops._
import graphics.meshes.Cube
import graphics.MatrixTransformer
import console.AnsiConsumer

case class Polar(distance: Float, hAngle: Float, vAngle: Float)

case class TetrisDrawingTools(width: Int, height: Int) {
  val rasterizer = Canvas(width, height)
  def projector(camera: Polar, focus: Vector3) = {
    val cameraEuclidian = polarToEuclidian(camera)
    PerspectiveProjector(cameraEuclidian + focus, focus, Vector3(0, 1, 0), cameraEuclidian to focus, 0.5f * width.toFloat / height)
  }

  def reduceState(state: GameState)(camera: Polar, focus: Vector3): String = {
    val proj = projector(camera, focus)
    val (rowOffset, colOffset) = (0.5f - state.board.height.toFloat / 2, 0.5f - state.board.width.toFloat / 2)
    val blockTransformers = state.occupiedPoints.map {
      case GridPoint(row, col, shape) => (MatrixTransformer(Vector3(col + colOffset, row + rowOffset, 0)), shapeColor(shape))
    }
    val outlineTransformers = for {
      row <- Range(0, state.board.height)
      col <- Range(0, state.board.width)
    } yield MatrixTransformer(Vector3(col + colOffset, row + rowOffset, 0))

    val primitives = blockTransformers.flatMap {
      case (transformer, color) => proj(transformer(unitCube), color)
    } ++ outlineTransformers.flatMap(t => 
      proj.projectLines(t.transformLines(frontWall), Color(true, true, true, 0.5f))
    ) ++ outlineTransformers.flatMap(t =>
      proj.projectLines(t.transformLines(midLines), Color(false, true, false, 0.5f))
    ) ++ outlineTransformers.flatMap(t =>
      proj.projectLines(t.transformLines(backWall), Color(true, false, false, 0.5f))
    )

    AnsiConsumer(rasterizer(primitives)).toString
  }  
}

object TetrisDrawingTools {
  def polarToEuclidian(polar: Polar): Vector3 = {
    val Polar(distance, hAngle, vAngle) = polar
    val v = toRadians(vAngle)
    val h = toRadians(hAngle)
    val d = cos(v).toFloat * distance
    Vector3(sin(h).toFloat * d, sin(v).toFloat * distance, cos(h).toFloat * d)
  }

  def shapeColor(shape: TetrominoShape): Color = shape match {
    case IShape => Color(false, true, true, 1)
    case JShape => Color(false, false, true, 1)
    case LShape => Color(true, true, true, 1)
    case OShape => Color(true, true, false, 1)
    case SShape => Color(false, true, false, 1)
    case TShape => Color(true, false, true, 1)
    case ZShape => Color(true, false, false, 1)
  }

  val midLines = List(
    // (Vector3(0.5f, -0.5f, -0.5f), Vector3(0.5f, -0.5f, 0.5f)),
    // (Vector3(-0.5f, -0.5f, 0.5f), Vector3(-0.5f, -0.5f, -0.5f)),
    // (Vector3(0.5f, 0.5f, -0.5f), Vector3(0.5f, 0.5f, 0.5f)),
    // (Vector3(-0.5f, 0.5f, 0.5f), Vector3(-0.5f, 0.5f, -0.5f)),
  )

  val backWall = List(
    (Vector3(-0.5f, -0.5f, -0.5f), Vector3(0.5f, -0.5f, -0.5f)),
    (Vector3(-0.5f, 0.5f, -0.5f), Vector3(0.5f, 0.5f, -0.5f)),
    (Vector3(-0.5f, -0.5f, -0.5f), Vector3(-0.5f, 0.5f, -0.5f)),
    (Vector3(0.5f, -0.5f, -0.5f), Vector3(0.5f, 0.5f, -0.5f)),
  )

  val frontWall = List(
    (Vector3(0.5f, -0.5f, 0.5f), Vector3(-0.5f, -0.5f, 0.5f)),
    (Vector3(0.5f, 0.5f, 0.5f), Vector3(-0.5f, 0.5f, 0.5f)),
    (Vector3(0.5f, -0.5f, 0.5f), Vector3(0.5f, 0.5f, 0.5f)),
    (Vector3(-0.5f, -0.5f, 0.5f), Vector3(-0.5f, 0.5f, 0.5f)),
  )

  val unitCube = Cube(1)
}