import shapeless._

case class SignalStatus(id: Int, name:String, status:String)
val ss = SignalStatus(1, "api.pimpay,ru", "ok")

trait Len[A] {
  def length(): Int
}

implicit def anyLen[A]:Len[A]  = () => 1
implicit val hnilLen:Len[HNil] = () => 0
implicit def hlistLen[H,T <: HList](implicit hLen:Len[H], tLen:Len[T]):Len[H :: T] = new Len[H :: T] {
  def length():Int = hLen.length() + tLen.length()
}

// внутри shapeless:
trait Generic[A] {
  type Repr
}

type GenericAux[A, R] = Generic[A] {type Repr = R}

// == Generic.Aux[SignalStatus, Int :: String :: String :: HNil]
val x = new Generic[SignalStatus] { type Repr = Int :: String :: String :: HNil }
val y: x.Repr = 1 :: "abc" :: "ok" :: HNil

// summoner
def length[A, R <: HList](a:A)(implicit gen:Generic.Aux[A, R], len:Len[R]):Int = len.length()


def length2[A, R <: HList](implicit gen:Generic.Aux[A, R], len:Len[R]):Int = len.length()

length((1,2,3))
// TODO: homework: length2[SignalStatus]()

case class Point(x:Int, y:Int)

length(Point(5,2)) // length2[Point]
length((4,5,6,"abc"))