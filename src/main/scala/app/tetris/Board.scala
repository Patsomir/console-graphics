package app.tetris

import scala.util.Try
import scala.util.Success

sealed trait BoardError
case object InvalidPlacement extends BoardError
case object OutOfBoardBounds extends BoardError

case class BoardPoint(row: Int, col: Int, tile: Option[TetrominoShape])

class Board private (width: Int, height: Int, tiles: Vector[Vector[Option[TetrominoShape]]]) {
  def slice(row: Int, col: Int, sliceWidth: Int, sliceHeight: Int): Vector[Vector[Option[TetrominoShape]]] =
    tiles.slice(col, col + sliceHeight).map(_.slice(row, row + sliceWidth))

  def get(row: Int, col: Int): Either[BoardError, Option[TetrominoShape]] = Try(tiles(row)(col)).toOption.toRight(OutOfBoardBounds)

  def place(tetromino: Tetromino)(row: Int, col: Int): Either[BoardError, Board] = {
    val points = tetromino.fold(List.empty[(Int, Int)]) {
      case (acc, TetrominoPoint(localRow, localCol, true)) => (row + localRow, col + localCol) :: acc
      case (acc, _) => acc
    }
    val newTiles = points.foldLeft(Right(tiles) : Either[BoardError, Vector[Vector[Option[TetrominoShape]]]]) {
      case (acc, (boardRow, boardCol)) =>
        Try(tiles(boardRow)(boardCol)) match {
          case Success(None) => acc.map(grid => grid.updated(boardRow, grid(boardRow).updated(boardCol, Some(tetromino.shape))))
          case _ => Left(InvalidPlacement)
        }
    }
    newTiles.map(Board(width, height, _))
  }

  def apply(row: Int, col: Int): Either[BoardError, Option[TetrominoShape]] = get(row, col)

  def fold[A](unit: A)(f: (A, BoardPoint) => A): A = tiles.map(_.zipWithIndex).zipWithIndex.foldLeft(unit) {
    case (acc, (row, rowIndex)) => row.foldLeft(acc) {
      case (rowAcc, (tile, colIndex)) => f(rowAcc, BoardPoint(rowIndex, colIndex, tile))
    }
  }
}

object Board {
  private def apply(width: Int, height: Int, tiles: Vector[Vector[Option[TetrominoShape]]]): Board = new Board(width, height, tiles)

  def empty(width: Int, height: Int): Board = Board(width, height, Vector.fill(height)(Vector.fill(width)(None)))
}