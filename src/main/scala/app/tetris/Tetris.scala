package app.tetris

sealed trait TetrisError
case object IllegalAction extends TetrisError

object Tetris {
  def move(state: GameState)(rowOffset: Int, colOffset: Int): Either[TetrisError, GameState] = {
    val GameState(board, tetromino, row, col) = state
    Either.cond(
      board.consistentWith(tetromino)(row + rowOffset, col + colOffset),
      GameState(board, tetromino, row + rowOffset, col + colOffset),
      IllegalAction
    )
  }

  private def moveFirst(state: GameState)(offsets: List[(Int, Int)]): Either[TetrisError, GameState] =
    offsets.foldLeft(Left(IllegalAction) : Either[TetrisError, GameState]) {
      case (acc, (rowOffset, colOffset)) => acc.orElse(move(state)(rowOffset, colOffset))
    }

  private def rotateRight(state: GameState): Either[TetrisError, GameState] = {
    val newState = state.copy(activeTetromino = state.activeTetromino.rotateRight)
    (state.activeTetromino.shape, state.activeTetromino.state) match {
      case(IShape, Spawn) => moveFirst(newState)(List((0, 0), (0, -2), (0, 1), (-1, -2), (2, 1)))
      case(IShape, Clockwise) => moveFirst(newState)(List((0, 0), (0, -1), (0, 2), (2, -1), (-1, 2)))
      case(IShape, Inverted) => moveFirst(newState)(List((0, 0), (0, 2), (0, -1), (1, 2), (-2, -1)))
      case(IShape, Counterclockwise) => moveFirst(newState)(List((0, 0), (0, 1), (0, -2), (-2, 1), (1, -2)))
      case(OShape, _) => move(newState)(0, 0)
      case(_, Spawn) => moveFirst(newState)(List((0, 0), (0, -1), (1, -1), (-2, 0), (-2, -1)))
      case(_, Clockwise) => moveFirst(newState)(List((0, 0), (0, 1), (-1, 1), (2, 0), (2, 1)))
      case(_, Inverted) => moveFirst(newState)(List((0, 0), (0, 1), (1, 1), (-2, 0), (-2, 1)))
      case(_, Counterclockwise) => moveFirst(newState)(List((0, 0), (0, -1), (-1, -1), (2, 0), (2, -1)))
    }
  }

  private def rotateLeft(state: GameState): Either[TetrisError, GameState] = {
    val newState = state.copy(activeTetromino = state.activeTetromino.rotateLeft)
    (state.activeTetromino.shape, state.activeTetromino.state) match {
      case(IShape, Clockwise) => moveFirst(newState)(List((0, 0), (0, 2), (0, -1), (1, 2), (-2, -1)))
      case(IShape, Inverted) => moveFirst(newState)(List((0, 0), (0, 1), (0, -2), (-2, 1), (1, -2)))
      case(IShape, Counterclockwise) => moveFirst(newState)(List((0, 0), (0, -2), (0, 1), (-1, -2), (2, 1)))
      case(IShape, Spawn) => moveFirst(newState)(List((0, 0), (0, -1), (0, 2), (2, -1), (-1, 2)))
      case(OShape, _) => move(newState)(0, 0)
      case(_, Clockwise) => moveFirst(newState)(List((0, 0), (0, 1), (-1, 1), (2, 0), (2, 1)))
      case(_, Inverted) => moveFirst(newState)(List((0, 0), (0, -1), (1, -1), (-2, 0), (-2, -1)))
      case(_, Counterclockwise) => moveFirst(newState)(List((0, 0), (0, -1), (-1, -1), (2, 0), (2, -1)))
      case(_, Spawn) => moveFirst(newState)(List((0, 0), (0, 1), (1, 1), (-2, 0), (-2, 1)))
    }
  }

  private def place(state: GameState)(newTetromino: Tetromino): Either[TetrisError, GameState] = {
    val GameState(board, tetromino, row, col) = state
    board.place(tetromino)(row, col).fold(
      _ => Left(IllegalAction),
      newBoard => Right(state.copy(board = newBoard).newActiveTetromino(newTetromino))
    )
  }

  def apply(state: GameState, action: GameAction): Either[TetrisError, GameState] =
    (state, action) match {
      case (GameState(_, _, row, col), MoveLeft) => move(state)(0, -1)
      case (GameState(_, _, row, col), MoveRight) => move(state)(0, 1)
      case (GameState(_, _, row, col), MoveDown) => move(state)(-1, 0)
      case (_, RotateLeft) => rotateLeft(state)
      case (_, RotateRight) => rotateRight(state)
      case (_, Place(newTetromino)) => place(state)(newTetromino)
    }
  
  def ifLegal(state: GameState, action: GameAction): GameState = apply(state, action).getOrElse(state)
}