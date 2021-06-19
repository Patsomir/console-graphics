package app.tetris

case class GridPoint[A](row: Int, col: Int, tile: A)

trait Grid[A] {
  def tiles: Seq[Seq[A]]
  def fold[B](unit: B)(f: (B, GridPoint[A]) => B): B = tiles.map(_.zipWithIndex).zipWithIndex.foldLeft(unit) {
    case (acc, (row, rowIndex)) => row.foldLeft(acc) {
      case (rowAcc, (tile, colIndex)) => f(rowAcc, GridPoint(rowIndex, colIndex, tile))
    }
  }
  def pointsWith(p: GridPoint[A] => Boolean): List[GridPoint[A]] =
    fold(List.empty[GridPoint[A]])((acc, point) => if(p(point)) point :: acc else acc)

  def mapPointsWith[B](f: GridPoint[A] => Option[B]): List[B] =
    fold(List.empty[B])((acc, point) => f(point) match {
      case Some(value) => value :: acc
      case None => acc
    })
}