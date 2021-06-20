package app.tetris

import graphics.Canvas
import java.io.BufferedWriter
import java.io.OutputStreamWriter
import java.io.FileOutputStream
import math.Vector3
import scala.concurrent.ExecutionContext
import java.util.concurrent.ForkJoinPool
import scala.concurrent.Future
import scala.annotation.tailrec
import scala.util.Random


object TetrisAppUtils {
  sealed trait Input
  case class GameInput(action: GameAction) extends Input
  case class CameraInput(offset: Polar) extends Input
  case object Exit extends Input

  case class AppState(game: GameState, camera: Polar, seed: Int)
  object AppState {
    def initial(tetrisRows: Int, tetrisCols: Int, seed: Int): AppState = {
      val (tetromino, newSeed) = newTetromino(seed)
      AppState(GameState.newGameState(tetrisCols, tetrisRows, tetromino), Polar(10.5f, 0, 0), newSeed)
    }
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(new ForkJoinPool)
  val out = new BufferedWriter(
    new OutputStreamWriter(new FileOutputStream(java.io.FileDescriptor.out), "ASCII"),
    2048
  )

  def bufferedPrintLn(s: String): Unit = {
    out.write(s + "\n")
    out.flush()
  }

  val rotateStep = 10.0f
  val zoomStep = 2.0f
  def parseInput(str: String): Option[Input] = str match {
    case "a" => Some(GameInput(MoveLeft))
    case "d" => Some(GameInput(MoveRight))
    case "s" => Some(GameInput(MoveDown))
    case "q" => Some(GameInput(RotateLeft))
    case "e" => Some(GameInput(RotateRight))
    case "j" => Some(CameraInput(Polar(0, -rotateStep, 0)))
    case "l" => Some(CameraInput(Polar(0, rotateStep, 0)))
    case "k" => Some(CameraInput(Polar(0, 0, -rotateStep)))
    case "i" => Some(CameraInput(Polar(0, 0, rotateStep)))
    case "o" => Some(CameraInput(Polar(-zoomStep, 0, 0)))
    case "u" => Some(CameraInput(Polar(zoomStep, 0, 0)))
    case "exit" => Some(Exit)
    case _ => None
  }

  val tetrominos = Vector(Tetromino.I, Tetromino.J, Tetromino.L, Tetromino.O, Tetromino.S, Tetromino.T, Tetromino.Z)
  def newTetromino(seed: Int): (Tetromino, Int) = (tetrominos(new Random(seed).nextInt(tetrominos.length)), seed + 1)

  def resolveInput(state: AppState, input: Input): AppState = input match {
    case GameInput(MoveDown) => {
      val (tetromino, newSeed) = newTetromino(state.seed)
      AppState(Tetris(state.game, MoveDown).getOrElse(Tetris.ifLegal(state.game, Place(tetromino))), state.camera, newSeed)
    }
    case GameInput(action) => state.copy(game = Tetris.ifLegal(state.game, action))
    case CameraInput(Polar(distOffset, hOffset, vOffset)) => {
      val Polar(dist, h, v) = state.camera
      state.copy(camera = Polar(dist + distOffset, h + hOffset, v + vOffset))
    }
    case _ => state
  }

  def displayAppState(state: AppState)(implicit tools: TetrisDrawingTools): Unit = {
    out.write(tools.reduceState(state.game)(state.camera, Vector3.zero) + "\n")
    out.flush()
  }
}

object TetrisApp extends App {
  import TetrisAppUtils._

  val width = 160
  val height = 40
  implicit val tools = TetrisDrawingTools(width, height)

  val tetrisRows = 5
  val tetrisCols = 10
  val newGame = AppState.initial(tetrisRows, tetrisCols, System.nanoTime.toInt)
  val screen = Future(displayAppState(newGame))

  @tailrec
  def readAndWrite(state: AppState, screen: Future[Unit]): Unit = {
    val inputStr = Console.in.readLine().toString
    parseInput(inputStr) match {
      case None => readAndWrite(state, screen)
      case Some(Exit) => ()
      case Some(input) => {
        val newState = resolveInput(state, input)
        val newScreen = screen.map(_ => displayAppState(newState))
        readAndWrite(newState, newScreen)
      }
    }
  }

  readAndWrite(newGame, screen)
}