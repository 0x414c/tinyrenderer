import scala.{specialized ⇒ sp}


sealed trait NumberLike[@sp (Float, Int) A, @sp (Float, Int) B, @sp (Float, Int) C] {
  def add (x: A, y: B): C
  def sub (x: A, y: B): C
  def mul (x: A, y: B): C
  def div (x: A, y: B): C
  def unary_neg (x: A): A
}

object NumberLike {
  implicit object NumberLikeIntFloat extends NumberLike[Int, Float, Float] {
    def add (x: Int, y: Float): Float = x + y
    def sub (x: Int, y: Float): Float = x - y
    def mul (x: Int, y: Float): Float = x * y
    def div (x: Int, y: Float): Float = x / y
    def unary_neg (x: Int): Int = -x
  }

  implicit object NumberLikeFloatInt extends NumberLike[Float, Int, Float] {
    def add (x: Float, y: Int): Float = x + y
    def sub (x: Float, y: Int): Float = x - y
    def mul (x: Float, y: Int): Float = x * y
    def div (x: Float, y: Int): Float = x / y
    def unary_neg (x: Float): Float = -x
  }

  implicit object NumberLikeIntInt extends NumberLike[Int, Int, Int] {
    def add (x: Int, y: Int): Int = x + y
    def sub (x: Int, y: Int): Int = x - y
    def mul (x: Int, y: Int): Int = x * y
    def div (x: Int, y: Int): Int = x / y
    def unary_neg (x: Int): Int = -x
  }

  implicit object NumberLikeFloatFloat extends NumberLike[Float, Float, Float] {
    def add (x: Float, y: Float): Float = x + y
    def sub (x: Float, y: Float): Float = x - y
    def mul (x: Float, y: Float): Float = x * y
    def div (x: Float, y: Float): Float = x / y
    def unary_neg (x: Float): Float = -x
  }
}
