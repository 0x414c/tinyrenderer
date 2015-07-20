object Utils {
  def mkStringFrom (from: Traversable[Any], sep: String = " "): String = (from.head.toString /: from.tail) ((acc, it) ⇒ acc + sep + it.toString)

  def mkString (sep: String, args: Any*): String = {
    mkStringFrom (args, sep)
  }

  def writeLinesFrom (from: Traversable[Any]): Unit = for (it ← from) println (it.toString)

  def writeLines (args: Any*): Unit = writeLinesFrom (args)

  def swapped[A, B] (a: A, b: B): (B, A) = (b, a)

  def timer (description: String = "", times: Int = 1, code: ⇒ Unit): Unit = {
    val start_time: Long = System.nanoTime
    var i: Int = 0
    while (i < times) {
      code
      i += 1
    }
    val current_time: Long = System.nanoTime
    val time_diff: Long = current_time - start_time
    Utils.writeLines(
      f"*  `$description%s` finished",
      f"   time total  `${time_diff}%d`ns (`${time_diff / 1000000000L: Double}%f`s)",
      f"   total calls `${times}%d`",
      f"   time avg    `${time_diff / times: Double}%f`ns (`${(time_diff / times: Double) / 1000000000L: Double}%f`s)\n"
    )
  }
}


object Utils_Test extends Test {
  code = () ⇒ {
    val testSeq = Vector (1, 2, 3, 4)
    val testSeq_2 = Seq (5, 6)
    val testSeq_3 = Seq (7)

    Utils.writeLines (
      testSeq.mkString,
      Utils.mkStringFrom (testSeq),
      Utils.mkStringFrom (testSeq, " :: "),
      Utils.mkStringFrom (testSeq_2),
      Utils.mkStringFrom (testSeq_2, "|"),
      Utils.mkStringFrom (testSeq_3),
      Utils.mkStringFrom (testSeq_3, "."),
      Utils.mkString (sep = " || ", 5, 6, 7, 8)
    )
  }
}
