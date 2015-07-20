import scala.{specialized ⇒ spec}


case class Dyad[@spec (Float, Int) +T] (_1: T, _2: T) extends Product2[T, T] {
  def swap: Dyad[T] = new Dyad[T] (this._2, this._1)

  override def toString = s"Dyad<${this._1}, ${this._2}>"
}


case class Pair[+T1, +T2] (_1: T1, _2: T2) extends Product2[T1, T2] {
  def swap: Pair[T2, T1] = new Pair[T2, T1] (this._2, this._1)

  override def toString = s"Pair<${this._1}, ${this._2}>"
}


case class Triad[@spec (Float, Int) +T] (_1: T, _2: T, _3: T) extends Product3[T, T, T] {
  override def toString = s"Triad<${this._1}, ${this._2}, ${this._3}>"
}


case class Triple[+T1, +T2, +T3] (_1: T1, _2: T2, _3: T3) extends Product3[T1, T2, T3] {
  override def toString = s"Triple<${this._1}, ${this._2}, ${this._3}>"
}


case class Quadriad[@spec (Float, Int) +T1] (_1: T1, _2: T1, _3: T1, _4: T1) extends Product4[T1, T1, T1, T1] {
  override def toString = s"Quadriad<${this._1}, ${this._2}, ${this._3}, ${this._4}>"
}


case class Quadriple[+T1, +T2, +T3, +T4] (_1: T1, _2: T2, _3: T3, _4: T4) extends Product4[T1, T2, T3, T4] {
  override def toString = s"Quadriple<${this._1}, ${this._2}, ${this._3}, ${this._4}>"
}
