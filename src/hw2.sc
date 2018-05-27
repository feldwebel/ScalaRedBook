object Lesson {

  def trim(s:String):String = s.trim
  def upperCase(s:String):String = s.toUpperCase
  def surround(dec:String) = dec + (_:String) + dec

  val result1 = surround("+++")(trim(upperCase("   HelLo   ")))

  // ???
  class Wrap[A] (val x: A){
    def function[A](f: A => A): A = f(x)
    def chainWith[A](f: A => A): A = ???
    def andChainWith[A](f: A => A): A = ???
    def runWith[A](s: A): A = ???
  }

  object Wrap {
    def apply[A](f: A => A) = new Wrap(f)
  }

  trait funct[A] {
    def f
  }

  class function[A] (f: A => A) extends funct[A] {
    override def f: A => A = f
  }



  val result2 = (
    Wrap function trim
      chainWith     upperCase
      andChainWith  surround("+++")
      runWith       "   HelLo   "
    )

  result1 = result2 // TRUE!
}