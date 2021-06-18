package app.tetris

case class GameState(baord: Board, activeTetramino: Tetromino, cursorRow: Int, cursorCol: Int)

object GameState {
  def newGameState(width: Int, height: Int, tetromino: Tetromino): GameState =
    GameState(Board.empty(width, height), tetromino, height, width / 2 - tetromino.size / 2)
}