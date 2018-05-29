object Lesson {

  def trim(s:String):String = s.trim
  def upperCase(s:String):String = s.toUpperCase
  def surround(dec:String) = dec + (_:String) + dec

  val result1 = surround("+++")(trim(upperCase("   HelLo   ")))

  // ???
  class Wrap[A](var f: A => A) {
    def chainWith(f1: A => A) = {
      this.f = f1.compose(f)
      this
    }
    def andChainWith(f2: A => A) = {
      this.f = f2.compose(f)
      this
    }
    def runWith(s: A): A = {
      f(s)
    }
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

  result1 == result2 // TRUE!*/
}