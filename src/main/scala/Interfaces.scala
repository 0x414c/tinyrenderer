import scala.collection.mutable.ArrayBuffer


trait Image2D {
  def width: Int
  def height: Int

  def flipVertical (): Image2D
  def flipHorizontal (): Image2D

  def save (path: String): Image2D
  def load (path: String): Image2D
}


trait RenderTarget2D extends Image2D {
  def set (x: Int, y: Int, color: Int): RenderTarget2D
  def fill (x0: Int, y0: Int, x1: Int, y1: Int, colorId: Int): RenderTarget2D
}


trait Texture2D extends Image2D {
  def get (x: Int, y: Int): Int
}


trait Drawer[T] {
  def line (target: T, x0: Int, y0: Int, x1: Int, y1: Int, color: Int): Unit
}


trait Model {
  var v: ArrayBuffer[Vector3[Float]]
  var vt: ArrayBuffer[Vector3[Float]]
  var vn: ArrayBuffer[Vector3[Float]]
  var f: ArrayBuffer[Triad[Vector3[Int]]]
}


trait Scene[+T] extends scala.collection.mutable.IndexedSeq[Pair[Model, MaterialsContainer]] {
  val models: Array[Model]
  val maps: Array[MaterialsContainer]

  def apply (idx: Int): Pair[Model, MaterialsContainer]
}


trait Positionable {
  var position: Vector3[Float]
}


trait Directionable {
  var direction: Vector3[Float]
}


trait Colorable {
  var color: RGBAColor
}


trait Camera extends Positionable with Directionable {
  var view: Matrix2[Float]
  var proj: Matrix2[Float]
  var z_near: Float
  var z_far: Float
}


trait LightSource extends Directionable with Colorable {
  var intensity: Float
}


trait Buffer2D {
  val depth: Float
  def test (x: Int, y: Int, z: Float): Boolean
  def clear (): Buffer2D
  def toImage (z_near: Float = 0.0F, z_far: Float = 512.0F): Image2D
}


trait Renderer {
  var world: Scene[Model]
  var frame_buffer: RenderTarget2D
  var z_buffer: Buffer2D

  def renderWireframe (color: RGBAColor): Renderer
  def renderFlat (color: RGBAColor): Renderer
  def clear (color: RGBAColor): Renderer
}


trait FragmentShader extends ((Int, Int) ⇒ RGBAColor) {
  // TODO:
}
