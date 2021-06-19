package app.tetris

case class GameState(board: Board, activeTetromino: Tetromino, cursorRow: Int, cursorCol: Int) {
  def newActiveTetromino(tetromino: Tetromino): GameState =
    copy(cursorRow = board.width / 2 - tetromino.size / 2, cursorCol = board.height)

  def occupiedPoints: List[GridPoint[TetrominoShape]] =
    board.mapPointsWith(_ match {
      case GridPoint(row, col, Some(shape)) => Some(GridPoint(row, col, shape))
      case _ => None
    }) ++ activeTetromino.mapPointsWith(_ match {
      case GridPoint(row, col, true) => Some(GridPoint(row, col, activeTetromino.shape))
      case _ => None
    })
}

object GameState {
  def newGameState(width: Int, height: Int, tetromino: Tetromino): GameState =
    GameState(Board.empty(width, height), tetromino, height, width / 2 - tetromino.size / 2)
}