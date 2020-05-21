trait HList
case class HCons[H, T <: HList](h:H, t:T) extends HList
case object HNil extends HList
type HNil = HNil.type

type ::[A, B <: HList] = HCons[A, B]

implicit class hlistOps[A <: HList](a:A) {
  def ::[B](b:B): HCons[B, A] = HCons(b, a)
}

val x: Int :: String :: HNil = 1 :: "abc" :: HNil
// equals to
val x2 = new hlistOps((new hlistOps(HNil).::("abc"))).::(1)

trait Len[A] {
  def length():Int
}

implicit def genLen[A]:Len[A]  = () => 1
implicit val hnilLen:Len[HNil] = () => 0

implicit def hlistLen[H,T <: HList](implicit hLen:Len[H], tLen:Len[T]):Len[H :: T] = new Len[H :: T] {
  def length():Int = hLen.length() + tLen.length()
}

def tuple2hlist[A,B](t: (A,B)): A :: B :: HNil = t._1 :: t._2 :: HNil
def tuple2hlist[A,B,C](t: (A,B,C)): A :: B :: C :: HNil = t._1 :: t._2 :: t._3 :: HNil

val t3 = (19, "api.pimpay.ru", false)
val t2 = (5,10)

// homework
def length[A <: HList](unused:A)(implicit l: Len[A]):Int = l.length()
def length2[A <: HList](implicit l: Len[A]):Int = l.length()
length ( tuple2hlist(t3) )

length2 [ Int :: Int :: HNil ]
length2 [ String :: Int :: Int :: Boolean :: HNil ]

length ( tuple2hlist(t3) )

length2 [ (Int :: Int :: HNil) :: String ::  Boolean :: HNil]
// for example:
case class Point(x:Int,y:Int)
case class Pixel(pos:Point, color:String, isTransparent: Boolean)