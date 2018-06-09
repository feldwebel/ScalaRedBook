object Lesson {

  def trim(s:String):String = s.trim
  def upperCase(s:String):String = s.toUpperCase
  def surround(dec:String) = dec + (_:String) + dec

  val result1 = surround("+++")(trim(upperCase("   HelLo   ")))

  // ???
  class Wrap[A](val f: A => A) {
    def chainWith(f1: A => A): Wrap[A] =
      new Wrap(f1 compose f)

    def andChainWith(f1: A => A): Wrap[A] =
      chainWith(f1)

    def runWith(s: A): A = f(s)
  }

  object Wrap {
    def function[A](f: A => A): Wrap[A] = new Wrap(f)
  }


  val result2 = (
    Wrap function trim
    chainWith     upperCase
    andChainWith  surround("+++")
    runWith       "   HelLo   "
  )

  result1 == result2 // TRUE!

  implicit class FuncSyntax[A](f: A => A) {
    def ~> (ff: A => A): Function1[A, A] = ff compose f
    def ! (s: A): A = f apply s
    //def apply(s: A): A = f(s)
  }

  val res3 = (trim _) ~> upperCase ~> surround("+++") ! ("  HelLo  ")

}