import scala.reflect.ClassTag
import scala.{specialized ⇒ sp}


class Matrix2[@sp (Float, Int) T: ClassTag] (val size: Int) {
  private val _contents: Array[T] = new Array[T] (size * size)

  def this (size: Int, args: T*) = {
    this (size)
    this.fill (args)
  }

  def this (row_1: Vector3[T], row_2: Vector3[T], row_3: Vector3[T]) = {
    this (9, row_1.x, row_1.y, row_1.z, row_2.x, row_2.y, row_2.z, row_3.x, row_3.y, row_3.z)
  }

  def apply (row: Int, col: Int): T = this._contents (row * size + col)
  def apply (idx: Int): T = this._contents (idx)

  def update (row: Int, col: Int, value: T): Unit = this._contents (row * size + col) = value
  def update (idx: Int, value: T): Unit = this._contents (idx) = value

  def fill (args: Seq[T]): this.type = {
    require (args.length <= this._contents.length)

    for (i ← args.indices) {
      this.update (i, args (i))
    }
    this
  }

  def *[@sp (Float, Int) B, @sp (Float, Int) C: ClassTag] (that: Matrix2[B]) (implicit tbc: NumberLike[T, B, C], ccc: NumberLike[C, C, C]): Matrix2[C] = {
    require (this.size == that.size)
    val C: Matrix2[C] = new Matrix2[C] (this.size)

    for (i ← 0 until this.size) {
      for (k ← 0 until this.size) {
        val t: T = this (i, k)
        for (j ← 0 until this.size) {
          C (i, j) = ccc.add (
            C (i, j),
            tbc.mul (
              t,
              that (k, j)
            )
          )
        }
      }
    }

//    var i: Int = 0; var j: Int = 0; var k: Int = 0; val size = this.size
//    while (i < size) {
//      while (k < size) {
//        val t: T = this (i, k)
//        while (j < size) {
////          println (i, k, j)
//          C (i, j) = ccc.add (
//            C (i, j),
//            tbc.mul (
//              t,
//              that (k, j)
//            )
//          )
//          j += 1
//        }
//        j = 0
//        k += 1
//      }
//      k = 0
//      i += 1
//    }

    C
  }

  override def toString = s"Matrix<${this.size} :: <${this._contents.mkString ("; ")}>>"
}


object Matrix_Test extends Test {
  code = () ⇒ {
    val order = 3
    val mat_1 = new Matrix2[Int] (size = order)
    val mat_2 = new Matrix2[Float] (size = order)

    mat_1 (1, 1) = 99

    for (i ← 0 until order * order)
      mat_1 (i) = i

    for (i ← 0 until order * order)
      mat_2 (i) = i + 1

    val mat_prod = mat_1 * mat_2

//    Utils.timer (
//      description = "Matrix2",
//      times = 1000000,
//      {
//        val mat_prod = mat_1 * mat_2
//      }
//    )

    Utils.writeLines (
      mat_1,
      mat_2,
      mat_prod
    )
  }
}
