package app.tetris

sealed trait GameAction
case object MoveLeft extends GameAction
case object MoveRight extends GameAction
case object MoveDown extends GameAction
case object RotateLeft extends GameAction
case object RotateRight extends GameAction
case class Place(newTetromino: Tetromino) extends GameAction