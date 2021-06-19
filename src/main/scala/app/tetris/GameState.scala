package app.tetris

case class GameState(board: Board, activeTetromino: Tetromino, cursorRow: Int, cursorCol: Int) {
  def newActiveTetromino(tetromino: Tetromino): GameState =
    copy(cursorRow = board.width / 2 - tetromino.size / 2, cursorCol = board.height)
}

object GameState {
  def newGameState(width: Int, height: Int, tetromino: Tetromino): GameState =
    GameState(Board.empty(width, height), tetromino, height, width / 2 - tetromino.size / 2)
}