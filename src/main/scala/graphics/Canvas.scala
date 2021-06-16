package graphics

class Canvas private (width: Int, height: Int) extends Rasterizer {

  type PixelGrid = Array[Array[RasterFragment]]
  case class PixelPosition(x: Int, y: Int, depth: Float)

  private def toPixelPosition(point: Point): PixelPosition = {
    def pixelIndex(size: Int, normalized: Float): Int = ((normalized + 1) / 2 * (size - 1)).round

    PixelPosition(pixelIndex(width, point.x), pixelIndex(height, point.y), point.z)
  }
  private def onCanvas(pixel: PixelPosition): Boolean = pixel.x >= 0 && pixel.x < width && pixel.y >= 0 && pixel.y < height && pixel.depth >= -1 && pixel.depth <= 1
  private def interpolate(pixel0: PixelPosition, pixel1: PixelPosition): List[PixelPosition] = {
    val xLength = pixel1.x - pixel0.x
    val yLength = pixel1.y - pixel0.y
    val zLength = pixel1.depth - pixel0.depth
    val totalPixels = (xLength.abs max yLength.abs) + 1

    (for {
      i <- Range(0, totalPixels)
      step = i.toFloat / (totalPixels - 1)
    } yield PixelPosition((pixel0.x + xLength * step).round, (pixel0.y + yLength * step).round, pixel0.depth + zLength * step)).toList
  }


  private def placePixel(grid: PixelGrid, pixel: PixelPosition , color: Color): Unit = 
    if(onCanvas(pixel) && grid(pixel.y)(pixel.x).depth > pixel.depth) grid(pixel.y)(pixel.x) = RasterFragment(color, pixel.depth)

  private def drawVertex(grid: PixelGrid, vertex: Vertex): PixelGrid = {
    val Vertex(point, color) = vertex
    placePixel(grid, toPixelPosition(point), color)
    grid
  }

  private def drawLine(grid: PixelGrid, line: Line): PixelGrid = {
    val Line(start, end, color) = line
    interpolate(toPixelPosition(start), toPixelPosition(end)).foreach(placePixel(grid, _, color))
    grid
  }

  private def drawTriangle(grid: PixelGrid, triangle: Triangle): PixelGrid = {
    val Triangle(a, b, c, color) = triangle
    val pixel0 = toPixelPosition(a)
    interpolate(toPixelPosition(b), toPixelPosition(c)).foreach(interpolate(pixel0, _).foreach(placePixel(grid, _, color)))
    val pixel1 = toPixelPosition(b)
    interpolate(toPixelPosition(a), toPixelPosition(c)).foreach(interpolate(pixel1, _).foreach(placePixel(grid, _, color)))
    val pixel2 = toPixelPosition(c)
    interpolate(toPixelPosition(b), toPixelPosition(a)).foreach(interpolate(pixel2, _).foreach(placePixel(grid, _, color)))
    grid
  }

  override def rasterize(primitives: List[Primitive]): Raster = new CanvasRaster(
    primitives.foldLeft(
      (for {
      _ <- Range(0, height)
      } yield Array.fill[RasterFragment](width)(RasterFragment(Color(false, false, false, 0), 2))).toArray
    ) {
      case (grid, vertex: Vertex) => drawVertex(grid, vertex)
      case (grid, line: Line) => drawLine(grid, line)
      case (grid, triangle: Triangle) => drawTriangle(grid, triangle)
    }
  )
}

object Canvas {
  def apply(width: Int, height: Int) = new Canvas(width, height)
}

class CanvasRaster private[graphics] (private val content: Array[Array[RasterFragment]]) extends Raster {
  override def foldLeft[A](unit: A)(f: (A, RasterFragment) => A): List[A] = content.toList.reverse.map(_.foldLeft(unit)(f))
  override def foldRight[A](unit: A)(f: (RasterFragment, A) => A): List[A] = content.toList.reverse.map(arr => arr.foldRight(unit)(f))
}

