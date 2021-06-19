package app.tetris

import scala.util.Try
import scala.util.Success

sealed trait BoardError
case object InvalidPlacement extends BoardError
case object OutOfBoardBounds extends BoardError

case class BoardPoint(row: Int, col: Int, tile: Option[TetrominoShape])

class Board private (tiles: Vector[Vector[Option[TetrominoShape]]]) {

  val height: Int = tiles.length
  val width: Int = tiles(0).length

  def slice(row: Int, col: Int, sliceWidth: Int, sliceHeight: Int): Vector[Vector[Option[TetrominoShape]]] =
    tiles.slice(col, col + sliceHeight).map(_.slice(row, row + sliceWidth))

  def get(row: Int, col: Int): Either[BoardError, Option[TetrominoShape]] = Try(tiles(row)(col)).toOption.toRight(OutOfBoardBounds)

  private def targetPoints(tetromino: Tetromino)(row: Int, col: Int): List[(Int, Int)] =
    tetromino.fold(List.empty[(Int, Int)]) {
      case (acc, TetrominoPoint(localRow, localCol, true)) => (row + localRow, col + localCol) :: acc
      case (acc, _) => acc
    }

  def canFit(tetromino: Tetromino)(row: Int, col: Int): Boolean = {
    val points = targetPoints(tetromino)(row, col)
    points.forall(
      (get _).tupled(_) match {
        case Right(None) => true
        case _ => false
      }
    )
  }

  def place(tetromino: Tetromino)(row: Int, col: Int): Either[BoardError, Board] = {
    val points = targetPoints(tetromino)(row, col)
    val newTiles = points.foldLeft(Right(tiles) : Either[BoardError, Vector[Vector[Option[TetrominoShape]]]]) {
      case (acc, (boardRow, boardCol)) =>
        Try(tiles(boardRow)(boardCol)) match {
          case Success(None) => acc.map(grid => grid.updated(boardRow, grid(boardRow).updated(boardCol, Some(tetromino.shape))))
          case _ => Left(InvalidPlacement)
        }
    }
    newTiles.map(Board(_))
  }

  def apply(row: Int, col: Int): Either[BoardError, Option[TetrominoShape]] = get(row, col)

  def fold[A](unit: A)(f: (A, BoardPoint) => A): A = tiles.map(_.zipWithIndex).zipWithIndex.foldLeft(unit) {
    case (acc, (row, rowIndex)) => row.foldLeft(acc) {
      case (rowAcc, (tile, colIndex)) => f(rowAcc, BoardPoint(rowIndex, colIndex, tile))
    }
  }
}

object Board {
  private def apply(tiles: Vector[Vector[Option[TetrominoShape]]]): Board = new Board(tiles)

  def empty(width: Int, height: Int): Board = Board(Vector.fill(height)(Vector.fill(width)(None)))
}