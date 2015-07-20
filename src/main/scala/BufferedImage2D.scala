import scala.language.implicitConversions
import java.awt.geom.AffineTransform
import java.awt.{Color, Graphics2D}
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.{File, IOException}
import javax.imageio.ImageIO


class RGBAColor (val r: Float = 0.0F, val g: Float = 0.0F, val b: Float = 0.0F, val a: Float = 1.0F) {
  def this (rgba: Int) = this (
      a = ((rgba >> 24) & 0xFF) / 255.0F,
      r = ((rgba >> 16) & 0xFF) / 255.0F,
      g = ((rgba >> 8) & 0xFF) / 255.0F,
      b = ((rgba >> 0) & 0xFF) / 255.0F
  )
  
//  def this (r: Double, g: Double, b: Double, a: Double = 1.0) = this (r.toFloat, g.toFloat, b.toFloat, a.toFloat)

  def this (brightness: Float) = this (brightness, brightness, brightness, 1.0F)

  def apply (): Int = this.toRGBA

  def toRGBA: Int = ((Maths.clamp (a) * 255).toInt << 24) | ((Maths.clamp (r) * 255).toInt << 16) | ((Maths.clamp (g) * 255).toInt << 8) | (Maths.clamp (b) * 255).toInt

  def avg: Float = (this.r + this.g + this.b) / 3.0F

  def darken (step: Float = 0.25F): RGBAColor = new RGBAColor (this.r - step, this.g - step, this.b - step, this.a)

  def lighten (step: Float = 0.25F): RGBAColor = new RGBAColor (this.r + step, this.g + step, this.b + step, this.a)

  def ** (that: Int): RGBAColor = new RGBAColor (this.r * that, this.g * that, this.b * that, this.a)

  def ** (that: Float): RGBAColor = new RGBAColor (this.r * that, this.g * that, this.b * that, this.a)

  def * (that: RGBAColor): RGBAColor = new RGBAColor (this.r * that.r, this.g * that.g, this.b * that.b, this.a)

  def ++ (that: Int): RGBAColor = new RGBAColor (this.r + that, this.g + that, this.b + that, this.a)

  def ++ (that: Float): RGBAColor = new RGBAColor (this.r + that, this.g + that, this.b + that, this.a)

  def + (that: RGBAColor): RGBAColor = new RGBAColor (this.r + that.r, this.g + that.g, this.b + that.b, this.a)

  def toHex: String = "RGBAColor<# " + ((Seq ((this.r * 255.0F).toInt, (this.g * 255.0F).toInt, (this.b * 255.0F).toInt) map ("%02X".format (_))) mkString " ") + ">"

  override def toString = s"RGBAColor<${this.r}, ${this.g}, ${this.b}, ${this.a}>"

  implicit def AWTColor2Color (from: RGBAColor): Color = new Color (from.r, from.g, from.b, from.a)
}


object RGBAColor {
  private val predef = Map[String, RGBAColor] (
    "red" → new RGBAColor (r = 1.0F),
    "green" → new RGBAColor (g = 1.0F),
    "blue" → new RGBAColor (b = 1.0F),
    "white" → new RGBAColor (1.0F, 1.0F, 1.0F),
    "black" → new RGBAColor (),
    "cyan" → new RGBAColor (0.0F, 1.0F, 1.0F),
    "magenta" → new RGBAColor (1.0F, 0.0F, 1.0F),
    "yellow" → new RGBAColor (1.0F, 1.0F, 0.0F)
  )
  
  def apply (name: String) = this.predef (name.toLowerCase)
}


class BufferedImage2D private () extends Image2D with RenderTarget2D with Texture2D {
  private var _image: BufferedImage = null
  def image: BufferedImage = this._image
  
  private var _width: Int = 0
  def width: Int = _width
  
  private var _height: Int = 0
  def height: Int = _height
  
  def this (width: Int, height: Int) = {
    this ()
    this._image = new BufferedImage (width, height, BufferedImage.TYPE_INT_ARGB)
    this._width = width
    this._height = height
  }
  
  def this (path: String) = {
    this ()
    this.load (path)
  }

  def set (x: Int, y: Int, color: Int): BufferedImage2D = {
    this._image.setRGB (x, y, color)

    this
  }

  def set (idx: Int, color: Int): BufferedImage2D = {
     this._image.setRGB (idx % width, idx / width, color)
//    this._image.setRGB (idx / height, idx % height, color)

    this
  }

  def get (x: Int, y: Int): Int = {
    this._image.getRGB (x, y)
  }

  def fill (x0: Int = 0, y0: Int = 0, x1: Int = this.image.getWidth, y1: Int = this.image.getHeight, color: Int = (new RGBAColor) ()): BufferedImage2D = {
//    for (x ← x0 until x1) for (y ← y0 until y1) this.image.setRGB (x, y, color)

    // HOT: java.awt.image.BufferedImage.createGraphics
    val graphics: Graphics2D = this._image.createGraphics
    graphics.setColor (new Color (color))
    graphics.fillRect (x0, y0, x1 - x0, y1 - y0)
    graphics.dispose ()

    this
  }

  def flipVertical (): BufferedImage2D = {
//    val tx = AffineTransform.getScaleInstance (1, -1)
//    tx.translate (0, -this.image.getHeight (null))
//    val op = new AffineTransformOp (tx, AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
//    this.image = op.filter (this.image, null)

    val flipped: BufferedImage = new BufferedImage (this.width, this.height, this.image.getType)
    val graphics: Graphics2D = flipped.createGraphics
    graphics.drawImage (
      this._image,
      this.width, this.height, 0, 0,
      this.width, 0, 0, this.height,
      null
    )
    this._image = flipped
    graphics.dispose ()

    this
  }

  def flipHorizontal (): BufferedImage2D = {
//    val tx = AffineTransform.getScaleInstance (-1, 1)
//    tx.translate (-this.image.getWidth (null), 0)
//    val op = new AffineTransformOp (tx, AffineTransformOp.TYPE_NEAREST_NEIGHBOR)
//    this.image = op.filter (image, null)

    val graphics: Graphics2D = this._image.createGraphics
    graphics.drawImage (
      this._image,
      0, 0, this.width, this.height,
      this.width, 0, 0, this.height,
      null
    )
    graphics.dispose ()

    this
  }

  def save (path: String): BufferedImage2D = {
    try {
      ImageIO.write (this._image, "PNG", new File (path + ".png"))
    } catch {
      case e: IOException ⇒ println (e.getMessage)
    }

    this
  }

  def load (path: String): BufferedImage2D = {
    try {
      this._image = ImageIO.read (new File (path))
      this._width = this._image.getWidth
      this._height = this._image.getHeight
    } catch {
      case e: IOException ⇒ println (e.getMessage); return this
    }

    this
  }

  override def toString = s"BufferedImage<${this.width}, ${this.height}, ${this._image.getType}>"
}


object BufferedImage2D {
  def fromFile (path: String) = new BufferedImage2D (path)
}


object BufferedImage2D_Test extends Test {
  code = () ⇒ {
    val dims = 10
    val testImage = new BufferedImage2D (width = dims, height = dims)
    val testColor = new RGBAColor (1.0F, 0.5F, 0.25F, 1.0F)

    Utils.writeLines (
      testImage,
      testColor,
      testColor.toHex
    )

    testImage.fill ()
    testImage.set (3, 5, testColor ())

    testImage.save (s"test_out\\${description}")
  }
}
