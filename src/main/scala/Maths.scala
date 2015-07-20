object Maths {
  object Limits {
    val EPS: Float = 0.00001F
    val min: Float = 0.0F - EPS
    val max: Float = 1.0F + EPS
  }
  
  // TODO: generics
  def clamp (value: Int, min: Int, max: Int): Int = {
    math.max (min, math.min (max, value))
//    if (value < min) {
//      min
//    } else {
//      if (value > max) {
//        max
//      } else {
//        value
//      }
//    }
  }

  // Aka saturate()
  def clamp (value: Float, min: Float = 0.0F, max: Float = 1.0F): Float = {
    math.max (min, math.min (max, value))
  }

  // Aka mix()
  def lerp (a: Float, b: Float, w: Float): Float = {
    (1.0F - w) * a + w * b
//    a + w * (b - a)
  }

  def min_3 (a: Int, b: Int, c: Int): Int = {
    math.min (a, math.min (b, c))
//    if (a > b) {
//      b
//    }
//    if (a > c) {
//      c
//    }
//    a
  }

  def max_3 (a: Int, b: Int, c: Int): Int = {
    math.max (a, math.max (b, c))
  }

  def interp_vector (bc: Vector3[Float], u: Vector3[Float], v: Vector3[Float], w: Vector3[Float]): Vector3[Float] = {
    u ** bc.x + v ** bc.y + w ** bc.z
  }

  def interp_scalar (bc: Vector3[Float], u: Float, v: Float, w: Float): Float = {
    u * bc.x + v * bc.y + w * bc.z
  }

  def triangleBoundingBox (a: Vector3[Int], b: Vector3[Int], c: Vector3[Int], constraint_w: Int, constraint_h: Int): Dyad[Range] = {
    val x_min = min_3 (a.x, b.x, c.x)
    val y_min = min_3 (a.y, b.y, c.y)
    val x_max = max_3 (a.x, b.x, c.x)
    val y_max = max_3 (a.y, b.y, c.y)

    new Dyad[Range] (x_min to x_max, y_min to y_max)
  }

  def triangleArea_unsignedDoubled (a: Vector3[Int], b: Vector3[Int], c: Vector3[Int]): Int = {
//    math.abs ((a.x * b.y + b.x * c.y + c.x * a.y) - (b.x * a.y + a.x * c.y + c.x * b.y)) // Area by Shoelace formula
    math.abs (a.x * (b.y - c.y) + a.y * (-b.x + c.x) - b.y * c.x + b.x * c.y) // Simplified
  }

  def isInside_byArea (abc_area: Int, a: Vector3[Int], b: Vector3[Int], c: Vector3[Int], x: Int, y: Int): Boolean = {
    val p = new Vector3[Int] (x, y, 0)

    abc_area == triangleArea_unsignedDoubled (p, a, b) + triangleArea_unsignedDoubled (p, b, c) + triangleArea_unsignedDoubled (p, c, a)
  }

  /**
   * Finds normalized 2d coordinates of point (x, y) (given in orthogonal system)
   * in following (may be skew) coordinate system:
   * ort X is defined by AB, and Y is defined by AC
   * and A will be the origin point
   * (A, B, C are also given in orthogonal).
   * @param a
   * @param b
   * @param c
   * @param x
   * @param y
   * @return
   */
  def toParametric (a: Vector3[Int], b: Vector3[Int], c: Vector3[Int], x: Int, y: Int): Vector3[Float] = {
    val d: Float = a.x * (b.y - c.y) + a.y * (-b.x + c.x) - b.y * c.x + b.x * c.y // == 2 * SignedArea[/\[abc]]
    val u: Float = (x * (c.y - a.y) + y * (a.x - c.x) - a.x * c.y + a.y * c.x) /  d
    val v: Float = (x * (b.y - a.y) + y * (a.x - b.x) - a.x * b.y + a.y * b.x) / -d
    val w: Float = u + v

    new Vector3 (u, v, w)
  }

  /**
   * Determines if point (x, y) lies in triangle, ...
   * @param tc
   * @param x
   * @param y
   * @return
   */
  def isInside_byParametric (tc: Vector3[Float], x: Int, y: Int): Boolean = {
    Limits.min <= tc.x && tc.x <= Limits.max && Limits.min <= tc.y && tc.y <= Limits.max && tc.z <= Limits.max
  }

  def toBarycentric (a: Vector3[Int], b: Vector3[Int], c: Vector3[Int], x: Int, y: Int): Vector3[Float] = {
    /*
        [ax, ay, 1]
      T=[bx, by, 1]; Abs[Det[T]] == 2 * Area[/\[abc]] == -ay bx + ax by + ay cx - by cx - ax cy + bx cy ==
        [cx, cy, 1]                                   == -by cx + ay (-bx + cx) + ax (by - cy) + bx cy
    */
//    val d_T: Float = (b.y - c.y) * (a.x - c.x) + (c.x - b.x) * (a.y - c.y)
//    val λ_1: Float = ((b.y - c.y) * (x - c.x) + (c.x - b.x) * (y - c.y)) / d_T
//    val λ_2: Float = ((c.y - a.y) * (x - c.x) + (a.x - c.x) * (y - c.y)) / d_T
    val d_T: Float = a.x * (b.y - c.y) + a.y * (-b.x + c.x) - b.y * c.x + b.x * c.y
    val λ_1: Float = (b.x * ( c.y - y) + b.y * (-c.x + x) + c.x * y - c.y * x) / d_T
    val λ_2: Float = (a.x * (-c.y + y) + a.y * ( c.x - x) - c.x * y + c.y * x) / d_T
    val λ_3: Float = 1.0F - λ_1 - λ_2

    new Vector3 (λ_1, λ_2, λ_3)
  }

  def isInside_byBarycentric (bc: Vector3[Float], x: Int, y: Int): Boolean = {
//    bc.x > min && bc.y > min && bc.z < max
    Limits.min <= bc.x && bc.x <= Limits.max && Limits.min <= bc.y && bc.y <= Limits.max && Limits.min <= bc.z && bc.z <= Limits.max
  }
}


object Maths_Test extends Test {
  code = () ⇒ {
    /*
       C┃╲
        ┃  ╲
        ┃    ╲    *O
      C2*    P*╲
        ┃   I*   ╲
        ┃          ╲
       A╚━━━━━*━━━━━━╲B
              B2
    */
    val a = new Vector3 (0, 0, 0)
    val b = new Vector3 (10000, 0, 0)
    val c = new Vector3 (0, 10000, 0)
    val i = new Vector3 (2929, 2929, 0)
    val o = new Vector3 (7500, 7500, 0)
    val p = new Vector3 (5000, 5000, 0)

    val bc = Maths.toBarycentric (a, b, c, i.x, i.y)
    val bc_a = Maths.toBarycentric (a, b, c, a.x, a.y)
    val bc_b = Maths.toBarycentric (a, b, c, b.x, b.y)
    val bc_c = Maths.toBarycentric (a, b, c, c.x, c.y)
    val bc_b_2 = Maths.toBarycentric (a, b, c, b.x / 2, b.y)
    val bc_c_2 = Maths.toBarycentric (a, b, c, c.x, c.y / 2)
    val bc_o = Maths.toBarycentric (a, b, c, o.x, o.y)
    val bc_p = Maths.toBarycentric (a, b, c, p.x, p.y)

    val is_bc = Maths.isInside_byBarycentric (bc, i.x, i.y)

    val pc = Maths.toParametric (a, b, c, i.x, i.y)
    val pc_a = Maths.toParametric (a, b, c, a.x, a.y)
    val pc_b = Maths.toParametric (a, b, c, b.x, b.y)
    val pc_c = Maths.toParametric (a, b, c, c.x, c.y)
    val pc_b_2 = Maths.toParametric (a, b, c, b.x / 2, b.y)
    val pc_c_2 = Maths.toParametric (a, b, c, c.x, c.y / 2)
    val pc_o = Maths.toParametric (a, b, c, o.x, o.y)
    val pc_p = Maths.toParametric (a, b, c, p.x, p.y)

    val is_pc = Maths.isInside_byParametric (pc, i.x, i.y)

    Utils.writeLines (
      i, a, b, c,
      "\n",
      ("i ", bc), ("o ", bc_o), ("p ", bc_p), ("a ", bc_a), ("b ", bc_b), ("c ", bc_c), ("b2", bc_b_2), ("c2", bc_c_2),
      is_bc,
      "\n",
      ("i", pc), ("o ", pc_o), ("p ", pc_p), ("a ", pc_a), ("b ", pc_b), ("c ", pc_c), ("b2", pc_b_2), ("c2", pc_c_2),
      is_pc
    )

    val dim = 32
    val a_2 = new Vector3 (0, 0, 0)
    val b_2 = new Vector3 (dim, 0, 0)
    val c_2 = new Vector3 (0, dim, 0)
    val img = new BufferedImage2D (dim, dim)
    val img_2 = new BufferedImage2D (dim, dim)
    for (x ← 0 until dim) {
      for (y ← 0 until dim) {
        val bc = Maths.toBarycentric (a_2, b_2, c_2, x, y)
        val pc = Maths.toParametric (a_2, b_2, c_2, x, y)
        val color = new RGBAColor (bc.x, bc.y, bc.z)
        val color_2 = new RGBAColor (pc.x, pc.y, 0)
        img.set (x, y, color ())
        img_2.set (x, y, color_2 ())
      }
    }
    img.flipVertical ().save (s"test_out\\${description}_bc")
    img_2.flipVertical ().save (s"test_out\\${description}_pc")
  }
}
