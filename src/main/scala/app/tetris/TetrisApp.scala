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

object AppUtils {
  val rng = new Random
  val out = new BufferedWriter(
    new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"),
    2048
  )
  def bufferedPrintLn(s: String): Unit = {
    out.write(s + "\n")
    out.flush()
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

  val width = 160
  val height = 40
  val focus = Vector3(0, 0, 0)
  val rotateStep = 10.0f
  val zoomStep = 2.0f

  val tools = TetrisDrawingTools(width, height)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool)

  val newGame = Future.successful((GameState.newGameState(10, 5, newTetramino), Polar(10.5f, 0, 0)))
  newGame.map {
    case (state, camera) => bufferedPrintLn(tools.reduceState(state)(camera, focus))
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
        case (state, camera) => bufferedPrintLn(tools.reduceState(state)(camera, focus))
      }
      readAndWrite(newState)
    }
  }

  readAndWrite(newGame)
}