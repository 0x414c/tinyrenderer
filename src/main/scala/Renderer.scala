import scala.language.postfixOps
import scala.collection.mutable.ArrayBuffer
import scala.io.Source


// NOTE: see full format description here: http://www.martinreddy.net/gfx/3d/OBJ.spec
class WavefrontObjFile private () extends Model {
  var v: ArrayBuffer[Vector3[Float]] = null
  var vt: ArrayBuffer[Vector3[Float]] = null
  var vn: ArrayBuffer[Vector3[Float]] = null
  var f: ArrayBuffer[Triad[Vector3[Int]]] = null

//  /** A number following the rules of `decimalNumber`, with the following optional additions:
//  *  - Preceded by a negative sign
//  *  - Followed by `e` or `E` and an optionally signed integer
//  *  - Followed by `f`, `f`, `d` or `D` (after the above rule, if both are used)
//  */
//  val floatingPointNumber = """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?"""
//
//  /** An integer, without sign or with a negative sign. */
//  val wholeNumber = """-?\d+"""
//
//  /** Number following one of these rules:
//  *  - An integer. For example: `13`
//  *  - An integer followed by a decimal point. For example: `3.`
//  *  - An integer followed by a decimal point and fractional part. For example: `3.14`
//  *  - A decimal point followed by a fractional part. For example: `.1`
//  */
//  val decimalNumber = """(\d+(\.\d*)?|\d*\.\d+)"""
//
//  val doubleMatcher = s"""v $floatingPointNumber $floatingPointNumber $floatingPointNumber""".r
//
//  val integerMatcher = s"""f $wholeNumber $wholeNumber $wholeNumber""".r

  def this (src: String) = {
    this ()
    load (src)
  }

// TODO: use parser combinators?
  def load (src: String) = {
    v = new ArrayBuffer[Vector3[Float]] ()
    vt = new ArrayBuffer[Vector3[Float]] ()
    vn = new ArrayBuffer[Vector3[Float]] ()
    f = new ArrayBuffer[Triad[Vector3[Int]]] ()
    for (line ← Source.fromFile (src).getLines map (_.split ("""\s+"""))) {
      line (0) match {
        case "v" ⇒ this.v += new Vector3[Float] (line (1).toFloat, line (2).toFloat, line (3).toFloat)
        case "vt" ⇒ this.vt += new Vector3[Float] (line (1).toFloat, line (2).toFloat, line (3).toFloat)
        case "vn" ⇒ this.vn += new Vector3[Float] (line (1).toFloat, line (2).toFloat, line (3).toFloat)
        case "f" ⇒ {
          val values = line.slice (1, 4) map (_.split ("/") map ("0" + _) map (_.toInt))
          // HACK: Int//Int can be found (missing value), so we prepend found value with "0" before casting it to Int
          // NOTE: not only Tris can be found, but also Quads (Int/Int/Int/Int)
          // TODO: but we can perform triangulation at parsing time // ???
          this.f += new Triad[Vector3[Int]] (
            new Vector3[Int] (values (0)(0), values (0)(1), values (0)(2)),
            new Vector3[Int] (values (1)(0), values (1)(1), values (1)(2)),
            new Vector3[Int] (values (2)(0), values (2)(1), values (2)(2))
          )
        }
//        case "#" ⇒ println ("WavefrontObjFile: " + line.mkString (" "))
        case _ ⇒ ()
      }
    }
    this
  }

  override def toString = s"WavefrontObjFile<${this.v.size}v, ${this.vt.size}vt, ${this.vn.size}vn, ${this.f.size}f>"
}


class WavefrontScene (val models: Array[Model], val maps: Array[MaterialsContainer]) extends Scene[WavefrontObjFile] {
  def apply (idx: Int): Pair[Model, MaterialsContainer] = new Pair[Model, MaterialsContainer] (this.models (idx), this.maps (idx))

  override def toString () = s"Model<${this.models.toString}>"

  override def length: Int = models.length

  override def update (idx: Int, elem: Pair[Model, MaterialsContainer]): Unit = {
    this.models (idx) = elem._1
    this.maps (idx) = elem._2
  }
}


class ZBuffer (val width: Int, val height: Int, val depth: Float = 512.0F) extends Buffer2D {
  private val _contents = new Array[Float] (width * height)

  def apply (idx: Int): Float = this._contents (idx)

//  def apply (x: Int, y: Int): Float = this._contents (x * height + y)
  def apply (x: Int, y: Int): Float = this._contents (x + y * width)

  def update (idx: Int, value: Float) = this._contents (idx) = value

//  def update (x: Int, y: Int, value: Float) = this._contents (x * height + y) = value
  def update (x: Int, y: Int, value: Float) = this._contents (x + y * width) = value

  def test (x: Int, y: Int, z: Float): Boolean = {
    if (z > this (x, y)) {
      this (x, y) = z
      true
    } else {
      false
    }
  }

  def clear (): Buffer2D = {
    var i = 0
    val l = this._contents.length
    while (i < l) {
      this._contents (i) = 0.0F
      i += 1
    }
    this
  }

  //    y = y0 + (y1 - y0) * ( (x - x0) / (x1 - x0) ) ==> (x - xmin) / (xmax - xmin)
  def toImage (z_near: Float, z_far: Float): Image2D = {
    val img = new BufferedImage2D (width, height)
    var i = 0
    val len = this._contents.length
    while (i < len) {
      val z = this._contents (i)
      val d = (z - z_near) / (z_far - z_near)
      img.set (i, (new RGBAColor (d)) ())
      i += 1
    }
    img
  }
}


class RenderingDevice (
                        var frame_buffer: RenderTarget2D, var world: Scene[Model],
                        var light_source: LightSource, var camera: Camera
                      ) extends Renderer {
  var z_buffer: Buffer2D = new ZBuffer(this.frame_buffer.width, this.frame_buffer.height).clear ()

  def renderWireframe (wireframeColor: RGBAColor): Renderer = {
    val color = wireframeColor ()
    for (thing ← this.world) {
      for (face ← thing._1.f par) {
        val u = this.frame_buffer.width - 1
        val v = this.frame_buffer.height - 1
        val Seq (d, e, f) = Seq (thing._1.v (face._1.x - 1), thing._1.v (face._2.x - 1), thing._1.v (face._3.x - 1)) map (_.toScreenSpace (u, v, 0))
        BresenhamDrawer.triangle (
          this.frame_buffer, d, e, f, color
        )
      }
    }    
    this
  }

  def renderFlat (diffuseColor: RGBAColor): Renderer = {
    val color = diffuseColor ()
    for (thing ← this.world par) {
      for (face ← thing._1.f par) {
        val vert_coords = new Triad[Vector3[Float]] (
          thing._1.v (face._1.x - 1),
          thing._1.v (face._2.x - 1),
          thing._1.v (face._3.x - 1)
        )
        val text_coords = new Triad[Vector3[Float]] (
          thing._1.vt (face._1.y - 1),
          thing._1.vt (face._2.y - 1),
          thing._1.vt (face._3.y - 1)
        )
        val vert_normals = new Triad[Vector3[Float]] (
          thing._1.vn (face._1.z - 1),
          thing._1.vn (face._2.z - 1),
          thing._1.vn (face._3.z - 1)
        )

        // NOTE: we don't need normalized vectors below:
        val normal = (vert_coords._3 - vert_coords._1) ^ (vert_coords._2 - vert_coords._1)
        val intensity = normal ~ this.camera.direction

        if (intensity > 0.0F) {
          BresenhamDrawer.triangleFilled (
            target = this.frame_buffer, z_buffer = this.z_buffer,
            vert_coords, text_coords, vert_normals, textures = thing._2,
            diffuse_color = color, light_source = this.light_source
          )
        }
      }
    }
    this
  }

  def clear (color: RGBAColor = new RGBAColor()) = {
    this.frame_buffer.fill (0, 0, frame_buffer.width, frame_buffer.height, color ())
    this.z_buffer.clear ()
    this
  }
}


class OrthographicCamera (var position: Vector3[Float], var direction: Vector3[Float]) extends Camera {
  var view: Matrix2[Float] = _
  var proj: Matrix2[Float] = _
  var z_far: Float = _
  var z_near: Float = _
}


class PerspectiveCamera (var position: Vector3[Float], var direction: Vector3[Float]) extends Camera {
  var view: Matrix2[Float] = _
  var proj: Matrix2[Float] = _
  var z_far: Float = _
  var z_near: Float = _
}


class DirectionalLight (
                        var direction: Vector3[Float],
                        var color: RGBAColor = new RGBAColor (1.0F, 1.0F, 1.0F),
                        var intensity: Float = 1.0F
                       ) extends LightSource {

}


// NOTE: it can contain only square 2d maps
class MaterialsContainer (diff_path: String, spec_path: String, nm_path: String, val dim: Int) {
  private val contents: Map[String, Texture2D] = Map (
    "diff" → new BufferedImage2D (width = dim, height = dim).load (diff_path).flipVertical,
    "spec" → new BufferedImage2D (width = dim, height = dim).load (spec_path).flipVertical,
    "nm" → new BufferedImage2D (width = dim, height = dim).load (nm_path).flipVertical
  )

  def apply (kind: String): Texture2D = this.contents (kind)
}


//object Shaders {
//  private val predef = Map[String, Function2[Vector3[Double], Int, Int]] (
////   "phong" →
//  )
//
//  def apply (name: String) = this.predef (name)
//}


object ZBuffer_Test extends Test {
  code = () ⇒ {
    val w = 1024
    val h = 768
    val b = new ZBuffer(w, h)
    b.clear ()
    b (w * h - 1) = 99
    val v = b (1023, 767)
    assert (99 == v)
  }
}


object Renderer_Test extends Test {
  code = () ⇒ {
    val testsCount = 10
    val width = 512
    val height = 512
    val z_near = 0.0F
    val z_far = 512.0F

//    val modelPath = "dragon2.obj"
//    val modelPath = "bunny_4kf_normalized.obj"
//    val modelPath = "teapot/teapot.obj"
//    val modelPath = "teapot/teapot_f.obj"
//    val modelPath = "head.obj"
    val modelPath = "obj/african_head.obj"
    val normMapPath = "obj/african_head_nm_tangent.png"
    val diffMapPath = "obj/african_head_diffuse.png"
    val specMapPath = "obj/african_head_spec.png"

    val modelPath_2 = "obj/african_head_eye_inner.obj"
    val normMapPath_2 = "obj/african_head_eye_inner_nm_tangent.png"
    val diffMapPath_2 = "obj/african_head_eye_inner_diffuse.png"
    val specMapPath_2 = "obj/african_head_eye_inner_spec.png"

    val modelPath_3 = "obj/african_head_eye_outer.obj"
    val normMapPath_3 = "obj/african_head_eye_outer_nm_tangent.png"
    val diffMapPath_3 = "obj/african_head_eye_outer_diffuse.png"
    val specMapPath_3 = "obj/african_head_eye_outer_spec.png"

    val render_target = new BufferedImage2D (width = width, height = height)
    
    val testObjFile = new WavefrontObjFile (modelPath)
    val testTextures = new MaterialsContainer (diffMapPath, specMapPath, normMapPath, dim = 1024)
    val testObjFile_2 = new WavefrontObjFile (modelPath_2)
    val testTextures_2 = new MaterialsContainer (diffMapPath_2, specMapPath_2, normMapPath_2, dim = 256)
    val testObjFile_3 = new WavefrontObjFile (modelPath_3)
    val testTextures_3 = new MaterialsContainer (diffMapPath_3, specMapPath_3, normMapPath_3, dim = 256)

    val world = new WavefrontScene (Array (testObjFile, testObjFile_2, testObjFile_3), Array (testTextures, testTextures_2, testTextures_3))

//    val dir_light = new DirectionalLight (direction = new Vector3[Double] (-0.5, -0.5, -1), color = new AWTColor (1.0, 0.75, 0.5))
//    val dir_light = new DirectionalLight (direction = new Vector3[Double] (-0.5, -0.5, -1), color = new AWTColor (1.0, 0.7, 0.0), intensity = 2.25)
    val dir_light = new DirectionalLight (direction = new Vector3[Float] (-0.5F, -0.5F, -1), color = new RGBAColor (1.0F, 1.0F, 1.0F), intensity = 1.0F)
    val or_cam = new OrthographicCamera (direction = new Vector3[Float] (0, 0, -1), position = new Vector3[Float] (0, 0, -1))
    
    val renderer = new RenderingDevice (render_target, world, dir_light, or_cam)

    val bg_col = new RGBAColor (r = 0.5F)
    val diff_col = new RGBAColor (1.0F, 1.0F, 1.0F)

    Utils.writeLines (
      testObjFile
    )

//    Utils.timer (
//      "wireframe",
//      testsCount,
//      {
//        renderer.clear ()
//        renderer.renderWireframe (diff_col)
//      }
//    )

//    render_target.flipVertical ().save (s"test_out\\${description}_wireframe")

    Utils.timer (
      "renderFlat",
      testsCount,
      {
        renderer.clear (bg_col)
        renderer.renderFlat (diff_col)
      }
    )

    renderer.frame_buffer.flipVertical ().save (s"test_out\\${description}_flat")
    renderer.z_buffer.toImage (z_near, z_far).flipVertical ().save (s"test_out\\${description}_z_buff")
  }
}
