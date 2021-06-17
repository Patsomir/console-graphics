package console

case class AnsiString(str: String) {
  def saveCursor: AnsiString = this + "\u001B[s"
  def restoreCursor: AnsiString = this + "\u001B[u"
  def eraseScreen: AnsiString = this + "\u001B[2J"
  def moveTo(row: Int, col: Int): AnsiString = this + s"\u001B[${row};${col}H"

  def bold: AnsiString = this + Console.BOLD
  def reset: AnsiString = this + Console.RESET

  def red: AnsiString = this + Console.RED
  def blue: AnsiString = this + Console.BLUE
  def green: AnsiString = this + Console.GREEN
  def cyan: AnsiString = this + Console.CYAN
  def magenta: AnsiString = this + Console.MAGENTA
  def yellow: AnsiString = this + Console.YELLOW
  def white: AnsiString = this + Console.WHITE
  def black: AnsiString = this + Console.BLACK

  def redB: AnsiString = this + Console.RED_B
  def blueB: AnsiString = this + Console.BLUE_B
  def greenB: AnsiString = this + Console.GREEN_B
  def cyanB: AnsiString = this + Console.CYAN_B
  def magentaB: AnsiString = this + Console.MAGENTA_B
  def yellowB: AnsiString = this + Console.YELLOW_B
  def whiteB: AnsiString = this + Console.WHITE_B
  def blackB: AnsiString = this + Console.BLACK_B

  def color(r: Boolean, g: Boolean, b: Boolean): AnsiString = (r, g, b) match {
    case (true, false, false) => red
    case (false, true, false) => green
    case (false, false, true) => blue
    case (true, true, false) => yellow
    case (true, false, true) => magenta
    case (false, true, true) => cyan
    case (true, true, true) => white
    case (false, false, false) => black
  }

  def colorB(r: Boolean, g: Boolean, b: Boolean): AnsiString = (r, g, b) match {
    case (true, false, false) => redB
    case (false, true, false) => greenB
    case (false, false, true) => blueB
    case (true, true, false) => yellowB
    case (true, false, true) => magentaB
    case (false, true, true) => cyanB
    case (true, true, true) => whiteB
    case (false, false, false) => blackB
  }

  def add(other: String): AnsiString = this + other

  def +(other: AnsiString): AnsiString = AnsiString(str + other.str)
  override def toString: String = str
}

object AnsiString {
  implicit def stringToAnsiString(str: String): AnsiString = AnsiString(str)
  def empty = AnsiString("")
}
