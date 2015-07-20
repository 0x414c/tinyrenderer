import scala.{specialized ⇒ sp}


// TODO: boxing is a _major_ bottleneck!
class Vector3[@sp (Float, Int) T] (val x: T, val y: T, val z: T) {
  // Transformations
  def toScreenSpace (u: Int, v: Int, w: Int) (implicit ev: T =:= Float): Vector3[Int] = {
    new Vector3[Int] (
//      round ((this.x + norm) * u / 2.0).toInt,
//      round ((this.y + norm) * v / 2.0).toInt,
//      round ((this.z + norm) * w / 2.0).toInt
      ((this.x + 1.0F) * u / 2.0F).toInt,
      ((this.y + 1.0F) * v / 2.0F).toInt,
      ((this.z + 1.0F) * w / 2.0F).toInt
    )
  }

  def toWorldSpace (w: Int, v: Int, u: Int) (implicit ev: T =:= Int): Vector3[Float] = {
    new Vector3[Float] (
      this.x * 2.0F / w - 1.0F,
      this.y * 2.0F / w - 1.0F,
      this.z * 2.0F / w - 1.0F
    )
  }

  // Conversions
  def toInt (implicit ev: T =:= Float): Vector3[Int] = {
    new Vector3[Int] (
//      round (this.x).toInt,
//      round (this.y).toInt,
//      round (this.z).toInt
      this.x.toInt,
      this.y.toInt,
      this.z.toInt
    )
  }

  def toFloat (implicit ev: T =:= Int): Vector3[Float] = {
    new Vector3[Float] (
      this.x.toFloat,
      this.y.toFloat,
      this.z.toFloat
    )
  }

  // Vector operations
  // Aka Euclidean norm or length
  def norm (implicit ttt: NumberLike[T, T, T]): T = {
    math.sqrt ((this ~! this).asInstanceOf[Double]).asInstanceOf[T]
//    math.sqrt (this ~ this)
  }

  def normalized (implicit ttt: NumberLike[T, T, T]): Vector3[T] = {
    this \\! this.norm
  }

  // Vector-Vector operations
  // Dot product
  def ~[@sp (Float, Int) B, @sp (Float, Int) C] (that: Vector3[B]) (implicit tbc: NumberLike[T, B, C], ccc: NumberLike[C, C, C]): C = {
    ccc.add (
      ccc.add (
        tbc.mul (this.x, that.x),
        tbc.mul (this.y, that.y)
      ),
      tbc.mul (this.z, that.z)
    )
  }

  //HACK:
  private def ~! (that: Vector3[T]) (implicit ttt: NumberLike[T, T, T]): T = {
    ttt.add (
      ttt.add (
        ttt.mul (this.x, that.x),
        ttt.mul (this.y, that.y)
      ),
      ttt.mul (this.z, that.z)
    )
  }

  // Cross product
  def ^[@sp (Float, Int) B, @sp (Float, Int) C] (that: Vector3[B]) (implicit tbc: NumberLike[T, B, C], ccc: NumberLike[C, C, C]): Vector3[C] = {
    new Vector3[C] (
      ccc.sub (
        tbc.mul (this.y, that.z),
        tbc.mul (this.z, that.y)
      ),
      ccc.sub (
        tbc.mul (this.z, that.x),
        tbc.mul (this.x, that.z)
      ),
      ccc.sub (
        tbc.mul (this.x, that.y),
        tbc.mul (this.y, that.x)
      )
    )
  }

  // Entrywise Vector-Vector operations
  def +[@sp (Float, Int) B, @sp (Float, Int) C] (that: Vector3[B]) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.add (this.x, that.x),
      tbc.add (this.y, that.y),
      tbc.add (this.z, that.z)
    )
  }

  def -[@sp (Float, Int) B, @sp (Float, Int) C] (that: Vector3[B]) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.sub (this.x, that.x),
      tbc.sub (this.y, that.y),
      tbc.sub (this.z, that.z)
    )
  }

  def *[@sp (Float, Int) B, @sp (Float, Int) C] (that: Vector3[B]) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.mul (this.x, that.x),
      tbc.mul (this.y, that.y),
      tbc.mul (this.z, that.z)
    )
  }

  def \[@sp (Float, Int) B, @sp (Float, Int) C] (that: Vector3[B]) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.div (this.x, that.x),
      tbc.div (this.y, that.y),
      tbc.div (this.z, that.z)
    )
  }

  // Entrywise Vector-Scalar operations
  def **[@sp (Float, Int) B, @sp (Float, Int) C] (that: B) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.mul (this.x, that),
      tbc.mul (this.y, that),
      tbc.mul (this.z, that)
    )
  }

  def \\[@sp (Float, Int) B, @sp (Float, Int) C] (that: B) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.div (this.x, that),
      tbc.div (this.y, that),
      tbc.div (this.z, that)
    )
  }

  // HACK:
  private def \\! (that: T) (implicit ttt: NumberLike[T, T, T]): Vector3[T] = {
    new Vector3[T] (
      ttt.div (this.x, that),
      ttt.div (this.y, that),
      ttt.div (this.z, that)
    )
  }

  def ++[@sp (Float, Int) B, @sp (Float, Int) C] (that: B) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.add (this.x, that),
      tbc.add (this.y, that),
      tbc.add (this.z, that)
    )
  }

  def --[@sp (Float, Int) B, @sp (Float, Int) C] (that: B) (implicit tbc: NumberLike[T, B, C]): Vector3[C] = {
    new Vector3[C] (
      tbc.sub (this.x, that),
      tbc.sub (this.y, that),
      tbc.sub (this.z, that)
    )
  }

  def unary_- () (implicit ev: NumberLike[T, T, T]): Vector3[T] = {
    new Vector3[T] (
      ev.unary_neg (this.x),
      ev.unary_neg (this.y),
      ev.unary_neg (this.z)
    )
  }

  override def toString = s"Vec3<$x, $y, $z>"
}


object Vector3_Test extends Test {
  code = () ⇒ {
    Utils.timer (
      description = "Maths",
      times = 10000000,
      code = {
        val testVec3 = new Vector3[Int] (1, 1, 1)
        val testVec3_2 = new Vector3[Float] (2, 3, 4)

        val sum = testVec3 + testVec3_2
        val diff = testVec3_2 - testVec3
        val prod = testVec3 * testVec3_2
        val div = testVec3 \ testVec3_2

        val scalarProd = testVec3 ** 42
        val scalarProd_2 = testVec3_2 ** 42
        val scalarDiv = testVec3 \\ 42
        val scalarDiv_2 = testVec3_2 \\ 42

        val dot = testVec3 ~ testVec3
        val dot_2 = testVec3_2 ~ testVec3_2
        val dot_mixed = testVec3 ~ testVec3_2
        val norm = testVec3.norm
        val norm_2 = testVec3_2.norm
        val normalized = testVec3.normalized
        val normalized_2 = testVec3_2.normalized

        val minus = -testVec3
        val minus_2 = -testVec3_2

        val fs = new Vector3[Float] (.123F, .456F, .789F)
        val ss = fs.toScreenSpace (255, 255, 255)
        val fs_2 = ss.toWorldSpace (255, 255, 255)
      }
    )


    val testVec3 = new Vector3[Int] (1, 1, 1)
    val testVec3_2 = new Vector3[Float] (2, 3, 4)

    val sum = testVec3 + testVec3_2
    val diff = testVec3_2 - testVec3
    val prod = testVec3 * testVec3_2
    val scalarProd = testVec3 ** 42
    val scalarProd_2 = testVec3_2 ** 42

    val normalized = testVec3.normalized

    val fs = new Vector3[Float] (.123F, .456F, .789F)
    val ss = fs.toScreenSpace (255, 255, 255)
    val fs_2 = ss.toWorldSpace (255, 255, 255)

    Utils.writeLines (
      "Vector3:",
      testVec3,
      testVec3_2,
      "Ops:",
      "sum: " + sum,
      "negSum: " + diff,
      "prod: " + prod,
      "scProd: " + scalarProd,
      "scProd_2: " + scalarProd_2,
      "normalized: " + normalized,
      "Transforms:",
      fs,
      ss,
      fs_2
    )
  }
}
