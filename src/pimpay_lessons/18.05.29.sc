object less3 {
  def debug(obj:Any): String = obj match {
    case o @ MyC(i, _) if i > 90 => "MYC " + i + o.s
    case o @ MyC(i, _) if i < 90 => "MYC<90 " + i + o.s
    case (a:Int, _) => "Tuple2(INT, _)"
    case (a, b) => "Tuple2(" + debug(a) + " , " + debug(b) + ")"
    case a: Int => s"INT: $a"
    case a: String => s"STRING: $a"
    case _ => "UNKNOWN"
  }

  debug(3)
  debug("zhopa")

  debug(("ooo", 88))
  debug((6, 88))

  debug(false)

  ("ooo", 6)._2

  case class MyC(i: Int, s: String)

  val myc = new MyC(88, "zhopa")

  debug(myc)




}