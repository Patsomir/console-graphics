package app.tetris

import scala.util.Try
import scala.util.Success
import scala.annotation.tailrec

sealed trait TetrominoState {
  def toRight: TetrominoState = this match {
    case Spawn => Clockwise
    case Clockwise => Inverted
    case Inverted => Counterclockwise
    case Counterclockwise => Spawn
  }

  def toLeft: TetrominoState = this match {
    case Spawn => Counterclockwise
    case Clockwise => Spawn
    case Inverted => Clockwise
    case Counterclockwise => Inverted
  }
}
case object Spawn extends TetrominoState
case object Clockwise extends TetrominoState
case object Inverted extends TetrominoState
case object Counterclockwise extends TetrominoState

sealed trait TetrominoShape
case object IShape extends TetrominoShape
case object JShape extends TetrominoShape
case object LShape extends TetrominoShape
case object OShape extends TetrominoShape
case object SShape extends TetrominoShape
case object TShape extends TetrominoShape
case object ZShape extends TetrominoShape

case class Tetromino private (
  val tiles: List[List[Boolean]], 
  val shape: TetrominoShape,
  val state: TetrominoState
) extends Grid[Boolean] {
  def rotateRight: Tetromino = {
    @tailrec
    def rotateHelper(rest: List[List[Boolean]], acc: List[List[Boolean]]): List[List[Boolean]] =
      if(rest.isEmpty) acc
      else rotateHelper(tiles.map(_.tail), tiles.map(_.head) :: acc)

    this.copy(tiles = rotateHelper(tiles, Nil), state = state.toRight)
  }

  def rotateLeft: Tetromino = {
    @tailrec
    def rotateHelper(rest: List[List[Boolean]], acc: List[List[Boolean]]): List[List[Boolean]] =
      if(rest.isEmpty) acc.reverse
      else rotateHelper(tiles.map(_.tail), tiles.map(_.head).reverse :: acc)

    this.copy(tiles = rotateHelper(tiles, Nil), state = state.toRight)
  }

  def size: Int = tiles.length
}

object Tetromino {
  private def apply(grid: List[List[Boolean]], shape: TetrominoShape, state: TetrominoState = Spawn): Tetromino = new Tetromino(grid, shape, state)

  val I = Tetromino(
    List(
      List(false, false, false, false),
      List(false, false, false, false),
      List(true, true, true, true),
      List(false, false, false, false),
    ),
    IShape
  )
  
  val J = Tetromino(
    List(
      List(false, false, false),
      List(true, true, true),
      List(true, false, false),
    ),
    JShape
  )
  
  val L = Tetromino(
    List(
      List(false, false, false),
      List(true, true, true),
      List(false, false, true),
    ),
    LShape
  )

  val O = Tetromino(
    List(
      List(true, true),
      List(true, true),
    ),
    OShape
  )

  val S = Tetromino(
    List(
      List(false, false, false),
      List(true, true, false),
      List(false, true, true),
    ),
    SShape
  )

  val T = Tetromino(
    List(
      List(false, false, false),
      List(true, true, true),
      List(false, true, false),
    ),
    LShape
  )

  val Z = Tetromino(
    List(
      List(false, false, false),
      List(false, true, true),
      List(true, true, false),
    ),
    LShape
  )
}