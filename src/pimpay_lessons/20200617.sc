import shapeless._
import scala.util.{Random => R}


case class Point(x:Int, y:Int)
case class Color(c:String) // сделайте одно из нескольких цветов
case class Pixel(pos:Point, c:Option[Color] = None) // None = transparent
case class Bitmap(pixels: Vector[Pixel])

// 1. type class
trait Rnd[A] {
  def gen():A
}

// 2. summoner
def random[A](implicit rnd:Rnd[A]):A = rnd.gen()

// 3. simple instances
implicit val intRnd:Rnd[Int] = () => R.nextInt()
implicit val strRnd:Rnd[String] = () => Vector.fill(5)(R.nextPrintableChar()).mkString
implicit val colorRnd:Rnd[Color] = () => Color(Array("red", "green", "blue").apply(Math.abs(R.nextInt()) % 3))
implicit def optRnd[A : Rnd]:Rnd[Option[A]] = () => if (R.nextInt() % 10 > 5) Option(implicitly[Rnd[A]].gen()) else Option.empty[A]
implicit def vecRnd[A : Rnd]:Rnd[Vector[A]] = () => Vector.fill(Math.abs(R.nextInt()) % 5 + 1)(implicitly[Rnd[A]].gen())

// int float option[A]


// 4. shapeless
// 4.1 hnil
implicit val hnilRnd: Rnd[HNil] = () => HNil

// 4.2 hcons
implicit def hconsRnd[H, T <: HList]
(implicit h:Rnd[H], t:Rnd[T]):Rnd[H :: T] = () => h.gen() :: t.gen()

// 4.3 recurse from CC
implicit def fromCC[CC <: Product, Repr <: HList]
(implicit gen:Generic.Aux[CC, Repr], rnd:Lazy[Rnd[Repr]]):Rnd[CC] = () => gen.from(rnd.value.gen())

//
{
  implicit val intRnd:Rnd[Int] = () => 5
  random[Bitmap]
}


5

// 1. type class

// simp
type Seed = Int
def random[A](implicit rnd:Rnd[A]):State[Seed,A] = ???
random[Bitmap].run(127271)