package app.tetris

import scala.util.Try
import scala.util.Success
import scala.annotation.tailrec

sealed trait TetrominoState {
  def toRight: TetrominoState = this match {
    case Up => Right
    case Right => Down
    case Down => Left
    case Left => Up
  }

  def toLeft: TetrominoState = this match {
    case Up => Left
    case Right => Up
    case Down => Right
    case Left => Down
  }
}
case object Up extends TetrominoState
case object Right extends TetrominoState
case object Down extends TetrominoState
case object Left extends TetrominoState

sealed trait TetrominoShape
case object IShape extends TetrominoShape
case object JShape extends TetrominoShape
case object LShape extends TetrominoShape
case object OShape extends TetrominoShape
case object SShape extends TetrominoShape
case object TShape extends TetrominoShape
case object ZShape extends TetrominoShape

case class Tetromino private (
  val grid: List[List[Boolean]], 
  val shape: TetrominoShape,
  val state: TetrominoState
) {
  def rotateRight: Tetromino = {
    @tailrec
    def rotateHelper(rest: List[List[Boolean]], acc: List[List[Boolean]]): List[List[Boolean]] =
      if(rest.isEmpty) acc
      else rotateHelper(grid.map(_.tail), grid.map(_.head) :: acc)

    this.copy(grid = rotateHelper(grid, Nil), state = state.toRight)
  }

  def rotateLeft: Tetromino = {
    @tailrec
    def rotateHelper(rest: List[List[Boolean]], acc: List[List[Boolean]]): List[List[Boolean]] =
      if(rest.isEmpty) acc.reverse
      else rotateHelper(grid.map(_.tail), grid.map(_.head).reverse :: acc)

    this.copy(grid = rotateHelper(grid, Nil), state = state.toRight)
  }
  
  def isOccupied(x: Int, y: Int): Boolean = Try(grid(y)(x)) match {
    case Success(true) => true
    case _ => false
  }

  def size: Int = grid.length
}

object Tetromino {
  private def apply(grid: List[List[Boolean]], shape: TetrominoShape, state: TetrominoState = Up): Tetromino = new Tetromino(grid, shape, state)

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