package console

import graphics.RasterFragment
import graphics.Color
import graphics.Raster

trait ConsolePalette {
  def stringify(color: Color): String
}

object ConsolePalette {
  implicit val defaultPalette: ConsolePalette = new ConsolePalette {
    private val chars = Array(' ', '`', '.', ':', '-', '~', '=', 'o', '*', '#', '%', '@', '&', '$', 'M', 'W');
    override def stringify(color: Color): String = {
      val Color(r, g, b, intensity) = color
      val clampedIntensity = 0.0 max intensity min 1.0
      AnsiString.empty.color(r, g, b).add(
        chars((0.5f + clampedIntensity * (chars.length - 1)).toInt).toString
      ).reset.toString
    }
  }
}

case object AnsiConsumer {
  def apply(raster: Raster)(implicit palette: ConsolePalette): AnsiString =
    raster.foldLeftUp(AnsiString.empty, (1, AnsiString.empty.eraseScreen))(
      {
        case (acc, RasterFragment(color, _)) => acc + palette.stringify(color)
      },
      {
        case ((index, acc), row) => (index + 1, acc.moveTo(index, 1) + row)
      }
    )._2
}
