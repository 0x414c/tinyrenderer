import java.awt.{Color, Graphics2D}

object BresenhamDrawer extends Drawer[RenderTarget2D] {
  def line (target: RenderTarget2D, x0: Int, y0: Int, x1: Int, y1: Int, color: Int): Unit = {
//    lineBresenham (target, x0, y0, x1, y1, color)
    lineDDA (target, x0, y0, x1, y1, color)
//    lineAWT (target, x0, y0, x1, y1, color)
  }

  def lineAWT (target: RenderTarget2D, x0: Int, y0: Int, x1: Int, y1: Int, color: Int): Unit = {
    val graphics: Graphics2D = target.asInstanceOf[BufferedImage2D].image.createGraphics
    graphics.setColor (new Color (color))
    graphics.drawLine (x0, y0, x1, y1)
    graphics.dispose ()
  }

  def lineBresenham (target: RenderTarget2D, x0: Int, y0: Int, x1: Int, y1: Int, color: Int): Unit = {
    val dx = Math.abs (x1 - x0) // deltas
    val dy = Math.abs (y1 - y0)
    val sx = if (x0 < x1) 1 else -1 // increment signs
    val sy = if (y0 < y1) 1 else -1

    var (x, y) = (x0, y0)
    var err = dx - dy
    var err2 = 0

    // By Alois Zingl (http://members.chello.at/easyfilter/bresenham.html)
    while (true) {
      target.set (x, y, color)

      if (x == x1 && y == y1) {
        return
      }

      err2 = 2 * err

      if (err2 >= -dy) {
        err -= dy
        x += sx
      }

      if (err2 <= dx) {
        err += dx
        y += sy
      }
    }
  }

  def lineDDA (target: RenderTarget2D, x0: Int, y0: Int, x1: Int, y1: Int, color: Int): Unit = {
    val dx = math.abs (x1 - x0)
    val dy = math.abs (y1 - y0)
    val ix = if (x0 < x1) 1 else -1
    val iy = if (y0 < y1) 1 else -1
    var (x, y) = (x0, y0)

    if (dx > dy) {
      val ierr2 = 2 * dy
      val ierr = ierr2 - 2 * dx
      var err = ierr2 - dx
      while (x != x1) {
        target.set (x, y, color)
        if (err > 0) {
          y += iy
          err += ierr
        } else {
          err += ierr2
        }
        x += ix
      }
      target.set (x, y, color)
    } else {
      val ierr2 = 2 * dx
      val ierr = ierr2 - 2 * dy
      var err = ierr2 - dy
      while (y != y1) {
        target.set (x, y, color)
        if (err > 0) {
          x += ix
          err += ierr
        } else {
          err += ierr2
        }
        y += iy
      }
      target.set (x, y, color)
    }
  }

  def point (target: RenderTarget2D, a: Vector3[Int], color: Int): Unit = {
    target.set (a.x, a.y, color)
  }

  def path (target: RenderTarget2D, a: Vector3[Int], b: Vector3[Int], color: Int): Unit = {
    line (target, a.x, a.y, b.x, b.y, color)
  }

  def rectangle (target: RenderTarget2D, a: Vector3[Int], b: Vector3[Int], color: Int): Unit = {
    line (target, a.x, a.y, a.x, b.y, color)
    line (target, a.x, a.y, b.x, a.y, color)
    line (target, b.x, b.y, a.x, b.y, color)
    line (target, b.x, b.y, b.x, a.y, color)
  }

  def triangle (target: RenderTarget2D, a: Vector3[Int], b: Vector3[Int], c: Vector3[Int], color: Int): Unit = {
    line (target, a.x, a.y, b.x, b.y, color)
    line (target, b.x, b.y, c.x, c.y, color)
    line (target, c.x, c.y, a.x, a.y, color)
  }

  // NOTE: The very core of the rendering pipeline. Maybe it needs decomposition?
  // TODO: fragment shaders
  def triangleFilled (
                       target: RenderTarget2D, z_buffer: Buffer2D,
                       vert_coords: Triad[Vector3[Float]], tex_coords: Triad[Vector3[Float]], normals: Triad[Vector3[Float]],
                       textures: MaterialsContainer, diffuse_color: Int, light_source: LightSource
                     ): Unit = {
    val Seq (a, b, c) = Seq (vert_coords._1, vert_coords._2, vert_coords._3) map (_.toScreenSpace (target.width - 1, target.height - 1, z_buffer.depth.toInt))
    val (u, v, w) = (tex_coords._1, tex_coords._2, tex_coords._3)
    val (r, s, t) = (normals._1, normals._2, normals._3)

    val triangle_area = Maths.triangleArea_unsignedDoubled (a, b, c)
    if (triangle_area > 0) {
      val bounding_box = Maths.triangleBoundingBox (a, b, c, target.width - 1, target.height - 1)
      for (x ← bounding_box._1) {
        for (y ← bounding_box._2) {
          if (Maths.isInside_byArea (triangle_area, a, b, c, x, y)) {
            val bc = Maths.toBarycentric (a, b, c, x, y)
            val z = Maths.interp_scalar (bc, a.z, b.z, c.z)
            if (z_buffer.test (x, y, z)) {
// NOTE: The reflection is calculated by taking the dot product of the surface's normal vector, N,
// and a normalized light-direction vector, L, pointing from the surface to the light source.
// This number is then multiplied by the color of the surface and the intensity of the light hitting the surface:
// I_d = L ~ N ** C ** I_l
// where I_d is the intensity of the diffusely reflected light (surface brightness), C is the color and
// I_l is the intensity of the incoming light. Because
// L * N = |N||L| cos(alpha) = cos(alpha),
// where alpha is the angle between the direction of the two vectors, the intensity will be the highest
// if the normal vector points in the same direction as the light vector (cos(0)=1,
// the surface will be perpendicular to the direction of the light),
// and the lowest if the normal vector is perpendicular to the light vector (cos(pi/2) = 0,
// the surface runs parallel with the direction of the light).

              var frag_color = new RGBAColor (diffuse_color)

              {
//                val intensity = ((vert_coords._3 - vert_coords._1) ^ (vert_coords._2 - vert_coords._1)).normalized ~ light_source.direction.normalized
//                frag_color = (frag_color ** intensity) * (light_source.color ** light_source.intensity)
              } // Flat shading

              {
//                val intensity = Maths.interpVector (bc, r, s, t) ~ light_source.direction.normalized
//                frag_color = (frag_color ** -intensity) * (light_source.color ** light_source.intensity)
              } // Smooth shading (Phong)

              {
                val intensity_1 = r ~ light_source.direction.normalized
                val intensity_2 = s ~ light_source.direction.normalized
                val intensity_3 = t ~ light_source.direction.normalized
                val intensity = Maths.interp_scalar (bc, intensity_1, intensity_2, intensity_3)
                frag_color = (frag_color ** -intensity) * (light_source.color ** light_source.intensity)
              } // Smooth shading (Gouraud)

              val tex_coord = (Maths.interp_vector (bc, u, v, w) ** textures.dim).toInt // TODO: we cannot handle non-square textures

              {
//                val normal_col = new RGBAColor (textures ("nm").get (tex_coord.x, tex_coord.y))
//                val normal_vec = new Vector3 (normal_col.r, normal_col.g, normal_col.b).normalized
//                val intensity = (Maths.interpVector (bc, r, s, t) * normal_vec) ~ light_source.direction.normalized
//                frag_color = frag_color ** -intensity
              } // Bumpmapping // TODO:

              {
//                frag_color *= new RGBAColor (textures ("diff").get (tex_coord.x, tex_coord.y))
              } // Texture

              {
//                frag_color += new RGBAColor (textures ("spec").get (tex_coord.x, tex_coord.y))
              } // Specular // TODO:

              target.set (x, y, frag_color ())
            }
          }
        }
      }
    }
  }
}


object BresenhamDrawer_Test extends Test {
  code = () ⇒ {
    val dimensions = 400
    val test_picture = new BufferedImage2D (width = dimensions, height = dimensions)
    val bg_color = new RGBAColor (1.0F, 1.0F, 1.0F)
    val fg_color = new RGBAColor ()
    val red_color = new RGBAColor (r = 1.0F)
    val blue_color = new RGBAColor (b = 1.0F)
    val black_color = RGBAColor ("black")
    val green_color = RGBAColor ("green")
    val yellow_color = RGBAColor ("yellow")
    val magenta_color = RGBAColor ("magenta")
    val cyan_color = RGBAColor ("cyan")

    test_picture.fill (0, 0, test_picture.width, test_picture.height, black_color ())

    //    // dx = dy
    //    BresenhamDrawer.line (test_picture, 0, 0, 2, 2, red_color ())
    //    BresenhamDrawer.line (test_picture, 2, 2, 0, 0, blue_color ())
    //
    //    // asymmetry
    ////    BresenhamDrawer.line (testPicture, 4, 0, 16, 2, redColor ())
    ////    BresenhamDrawer.line (testPicture, 16, 2, 4, 0, blueColor ())
    //    BresenhamDrawer.line (test_picture, 2 + 3, 0, 16 + 3, 2, red_color ())
    //    BresenhamDrawer.line (test_picture, 16 + 3, 2, 2 + 3, 0, blue_color ())
    //
    //    // dot
    //    BresenhamDrawer.line (test_picture, 64, 32, 64, 32, red_color ())
    //    BresenhamDrawer.line (test_picture, 64, 33, 64, 33, blue_color ())
    //
    //    // zero delta
    //    BresenhamDrawer.line (test_picture, 0, 4, 8, 4, red_color ())
    //    BresenhamDrawer.line (test_picture, 8, 4, 0, 4, blue_color ())
    //    BresenhamDrawer.line (test_picture, 32, 0, 32, 16, red_color ())
    //    BresenhamDrawer.line (test_picture, 32, 16, 32, 0, blue_color ())
    //
    //    // small delta
    //    BresenhamDrawer.line (test_picture, 32, 0, 33, 16, red_color ())
    //    BresenhamDrawer.line (test_picture, 33, 16, 32, 0, blue_color ())
    //
    //    // 2px wide
    //    BresenhamDrawer.line (test_picture, 64, 8, 65, 8, red_color ())
    //    BresenhamDrawer.line (test_picture, 65, 9, 64, 9, blue_color ())
    //
    //    // 2px tall
    //    BresenhamDrawer.line (test_picture, 84, 8, 84, 9, red_color ())
    //    BresenhamDrawer.line (test_picture, 85, 9, 85, 8, blue_color ())
    //
    //    // target bounds
    //    BresenhamDrawer.line (test_picture, 120, 180, test_picture.width - 1, test_picture.height - 1, fg_color ())
    //
    //    // intersection
    //    BresenhamDrawer.line (test_picture, 0, 10, 50, 50, fg_color ())
    //    BresenhamDrawer.line (test_picture, 10, 0, 50, 50, fg_color ())
    //
    //    // cross
    //    BresenhamDrawer.line (test_picture, 60, 60, 120, 180, blue_color ())
    //    BresenhamDrawer.line (test_picture, 120, 180, 60, 60, red_color ())
    //    BresenhamDrawer.line (test_picture, 120, 60, 60, 180, blue_color ())
    //    BresenhamDrawer.line (test_picture, 60, 180, 120, 60, red_color ())
    //
    //    // another cross
    //    BresenhamDrawer.line (test_picture, 199, 120, 0, 100, red_color ())
    //    BresenhamDrawer.line (test_picture, 0, 100, 199, 120, blue_color ())
    //    BresenhamDrawer.line (test_picture, 199, 100, 0, 120, red_color ())
    //    BresenhamDrawer.line (test_picture, 0, 120, 199, 100, blue_color ())

    val margin = dimensions / 24
    val xcenter = dimensions / 2
    val ycenter = dimensions / 2
    val line_length = dimensions / 2 - margin

    val n_lines = 24 * 6
    val angle_step = (2 * math.Pi) / n_lines

    for (n ← 0 to n_lines) {
      val theta = angle_step * n
      val x0 = (margin * math.cos (theta)).toInt + xcenter
      val y0 = (margin * math.sin (theta)).toInt + ycenter
      val x1 = (line_length * math.cos (theta)).toInt + xcenter
      val y1 = (line_length * math.sin (theta)).toInt + ycenter
      //      println (x0, y0, x1, y1)
      (() ⇒ {
        val eflaPainter = new EFLA
//        eflaPainter.drawLine (test_picture, x0, y0, x1, y1, yellow_color ())
//        eflaPainter.drawLine (test_picture, x1, y1, x0, y0, green_color.darker (.5)())
//        eflaPainter.drawLine (test_picture, x0, y0, x1, y1, red_color ())
//        eflaPainter.drawLine (test_picture, x1, y1, x0, y0, blue_color ())
      }) ()
      (() ⇒ {
        val ddaPainter = new DDA
//        ddaPainter.drawLine (test_picture, x0, y0, x1, y1, red_color ())
//        ddaPainter.drawLine (test_picture, x1, y1, x0, y0, blue_color ())
      }) ()
      (() ⇒ {
        val bresenhamPainter = new Bresenham
//        bresenhamPainter.drawLine (test_picture, x0, y0, x1, y1, red_color ())
//        bresenhamPainter.drawLine (test_picture, x1, y1, x0, y0, blue_color ())
      }) ()
      (() ⇒ {
//        BresenhamDrawer.line (test_picture, x0, y0, x1, y1, green_color ())
//        BresenhamDrawer.line (test_picture, x1, y1, x0, y0, cyan_color.darker(.5)())
//        BresenhamDrawer.drawLine (test_picture, x0, y0, x1, y1, red_color ())
//        BresenhamDrawer.drawLine (test_picture, x1, y1, x0, y0, blue_color ())
        BresenhamDrawer.lineDDA (test_picture, x0, y0, x1, y1, red_color ())
        BresenhamDrawer.lineDDA (test_picture, x1, y1, x0, y0, blue_color ())
      }) ()
      (() ⇒ {
        val graphics: Graphics2D = test_picture.image.createGraphics
//        graphics.setColor (new Color (red_color ()))
//        graphics.drawLine (x0, y0, x1, y1)
//        graphics.setColor (new Color (blue_color ()))
//        graphics.drawLine (x1, y1, x0, y0)
        graphics.dispose ()
      }) ()
    }

    test_picture.flipVertical ().save (s"test_out\\${description}_draw")

    test_picture.fill (color = red_color ())

    test_picture.save (s"test_out\\${description}_fill")
  }
}
