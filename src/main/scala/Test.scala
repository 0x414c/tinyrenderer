trait Test {
  val description = this.getClass.getCanonicalName.replaceAll("\\$", "_")

  def run (): Unit = {
    println(s"+  ${description}")
    code ()
    println(s"~  ${description}\n")
  }

  var code: () ⇒ Unit = () ⇒ ()
}
