package console

object AnsiOps {
  implicit class AnsiString(val str: String) {

    def saveCursor = this + "\u001B[s"
    def restoreCursor = this + "\u001B[u"
    def eraseScreen = this + "\u001B[2J"
    def moveTo(row: Int, col: Int) = this + s"\u001B[${row};${col}H"

    def bold = this + Console.BOLD
    def reset = this + Console.RESET
    
    def red = this + Console.RED
    def blue = this + Console.BLUE
    def green = this + Console.GREEN
    def cyan = this + Console.CYAN
    def magenta = this + Console.MAGENTA
    def yellow = this + Console.YELLOW
    def white = this + Console.WHITE
    def black = this + Console.BLACK

    def redB = this + Console.RED_B
    def blueB = this + Console.BLUE_B
    def greenB = this + Console.GREEN_B
    def cyanB = this + Console.CYAN_B
    def magentaB = this + Console.MAGENTA_B
    def yellowB = this + Console.YELLOW_B
    def whiteB = this + Console.WHITE_B
    def blackB = this + Console.BLACK_B

    def color(r: Boolean, g: Boolean, b: Boolean) = (r, g, b) match {
      case (true, false, false) => red
      case (false, true, false) => green
      case (false, false, true) => blue
      case (true, true, false) => yellow
      case (true, false, true) => magenta
      case (false, true, true) => cyan
      case (true, true, true) => white
      case (false, false, false) => black
    }

    def colorB(r: Boolean, g: Boolean, b: Boolean) = (r, g, b) match {
      case (true, false, false) => redB
      case (false, true, false) => greenB
      case (false, false, true) => blueB
      case (true, true, false) => yellowB
      case (true, false, true) => magentaB
      case (false, true, true) => cyanB
      case (true, true, true) => whiteB
      case (false, false, false) => blackB
    }

    def add(other: String) = this + other

    def +(other: AnsiString) = AnsiString(str + other.str)
    override def toString = str
  }

  object AnsiString {
    def empty = AnsiString("")
  }
}