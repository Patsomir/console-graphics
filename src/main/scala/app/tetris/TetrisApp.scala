package app.tetris

import graphics.Canvas
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import math.Vector3
import graphics.PerspectiveProjector
import graphics.MatrixTransformer
import console.AnsiConsumer
import math.MetricVectorSpace.ops._
import graphics.meshes.Cube
import graphics.Color
import graphics.Primitive
import ch.qos.logback.core.helpers.Transform
import scala.concurrent.ExecutionContext
import java.util.concurrent.ForkJoinPool
import scala.concurrent.Future
import scala.annotation.tailrec
import scala.util.Random
import scala.math.{ cos, sin, toRadians }

case class Polar(distance: Float, hAngle: Float, vAngle: Float)

object AppUtils {
  def shapeColor(shape: TetrominoShape): Color = shape match {
    case IShape => Color(false, true, true, 1)
    case JShape => Color(false, false, true, 1)
    case LShape => Color(true, true, true, 1)
    case OShape => Color(true, true, false, 1)
    case SShape => Color(false, true, false, 1)
    case TShape => Color(true, false, true, 1)
    case ZShape => Color(true, false, false, 1)
  }

  val rng = new Random

  val width = 160
  val height = 40
  val canvas = Canvas(width, height)

  val out = new BufferedWriter(
    new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"),
    width * height
  );

  def bufferedPrintLn(s: String): Unit = {
    out.write(s + "\n")
    out.flush()
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

  def reduceState(state: GameState)(cameraPos: Vector3, focus: Vector3): String = {
    val projector = PerspectiveProjector(cameraPos, focus, Vector3(0, 1, 0), cameraPos to focus, 0.5f * width.toFloat / height)
    val (rowOffset, colOffset) = (0.5f - state.board.height.toFloat / 2, 0.5f - state.board.width.toFloat / 2)
    val blockTransformers = state.occupiedPoints.map {
      case GridPoint(row, col, shape) => (MatrixTransformer(Vector3(col + colOffset, row + rowOffset, 0)), shapeColor(shape))
    }
    val outlineTransformers = for {
      row <- Range(0, state.board.height)
      col <- Range(0, state.board.width)
    } yield MatrixTransformer(Vector3(col + colOffset, row + rowOffset, 0))

    val primitives = blockTransformers.flatMap {
      case (transformer, color) => projector(transformer(unitCube), color)
    } ++ outlineTransformers.flatMap(t => 
      projector.projectLines(t.transformLines(frontWall), Color(true, true, true, 0.5f))
    ) ++ outlineTransformers.flatMap(t =>
      projector.projectLines(t.transformLines(midLines), Color(false, true, false, 0.5f))
    ) ++ outlineTransformers.flatMap(t =>
      projector.projectLines(t.transformLines(backWall), Color(true, false, false, 0.5f))
    )

    AnsiConsumer(canvas(primitives)).toString
  }

  def action(str: String): Option[GameAction] = str match {
    case "a" => Some(MoveLeft)
    case "d" => Some(MoveRight)
    case "s" => Some(MoveDown)
    case "q" => Some(RotateLeft)
    case "e" => Some(RotateRight)
    case _ => None
  }

  def cameraMovement(rotateStep: Float, zoomStep: Float, str: String): Polar = str match {
    case "j" => Polar(0, -rotateStep, 0)
    case "l" => Polar(0, rotateStep, 0)
    case "k" => Polar(0, 0, -rotateStep)
    case "i" => Polar(0, 0, rotateStep)
    case "o" => Polar(-zoomStep, 0, 0)
    case "u" => Polar(zoomStep, 0, 0)
    case _ => Polar(0, 0, 0)
  }

  def polarToEuclidian(polar: Polar): Vector3 = {
    val Polar(distance, hAngle, vAngle) = polar
    val v = toRadians(vAngle)
    val h = toRadians(hAngle)
    val d = cos(v).toFloat * distance
    Vector3(sin(h).toFloat * d, sin(v).toFloat * distance, cos(h).toFloat * d)
  }

  val tetrominos = Vector(Tetromino.I, Tetromino.J, Tetromino.L, Tetromino.O, Tetromino.S, Tetromino.T, Tetromino.Z)
  def newTetramino() = {
    tetrominos(rng.nextInt(tetrominos.length))
  }

  def resolveAction(state: GameState, action: GameAction): GameState = action match {
    case MoveDown => Tetris(state, MoveDown).getOrElse(Tetris.ifLegal(state, Place(newTetramino)))
    case _ => Tetris.ifLegal(state, action)
  }
}

object TetrisApp extends App {
  import AppUtils._

  val focus = Vector3(0, 0, 0)
  val rotateStep = 10.0f
  val zoomStep = 2.0f

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool)

  val newGame = Future.successful((GameState.newGameState(10, 5, newTetramino), Polar(10.5f, 0, 0)))
  newGame.map {
    case (state, camera) => bufferedPrintLn(reduceState(state)(polarToEuclidian(camera), focus))
  }

  def readAndWrite(state: Future[(GameState, Polar)]): Unit = {
    val command = Console.in.readLine().toString
    if(command != "exit") {
      val Polar(dOffset, hOffset, vOffset) = cameraMovement(rotateStep, zoomStep, command)
      val newState = (action(command) match {
        case Some(action) => state.map {
          case (s, camera) => (resolveAction(s, action), camera)
        }
        case None => state
      }).map {
        case (s, Polar(d, h, v)) => (s, Polar(d + dOffset, h + hOffset, v + vOffset))
      }
      newState.map {
        case (state, camera) => bufferedPrintLn(reduceState(state)(polarToEuclidian(camera), focus))
      }
      readAndWrite(newState)
    }
  }

  readAndWrite(newGame)
}