case class StatusSignal(id:Int, service:String, status:Boolean)
// (Int,String,Boolean)
// (Int,(String,(Boolean,HNil)))

// Product = HList (Heterogeneous List) // Homogeneous List
// Int :: String :: Boolean :: HNil
type ::[A,B] = (A,B)
type HNil = Unit
val HNil:HNil = ()

val tsss: Int :: String :: Boolean :: HNil = (19, ("api", (false, HNil)))
val tss: Int :: Int :: HNil = (2, (3, HNil))

// Pim format: csv-like format for encoding data
// eg. StatusSignal(19, "api", true) = 19:"api":F:END
trait PimEncoder[A] {
  def encode(a:A):String
}


def buildPimEncoder[A](f: A => String):PimEncoder[A] = (a:A) => f(a) // SAM

implicit val intPimEncoder:PimEncoder[Int]
= buildPimEncoder(_.toString)

implicit val strPimEncoder:PimEncoder[String]
= buildPimEncoder(s => "\"" + s + "\"") // TODO: correct enquote!

implicit val boolPimEncoder:PimEncoder[Boolean]
= buildPimEncoder(b => if (b) "T" else "F")

implicit val hnilPimEncoder:PimEncoder[HNil] = (_:HNil) => "END" // SAM

implicit def hlistEncoder[H,T](implicit headEncoder:PimEncoder[H], tailEncoder:PimEncoder[T]):PimEncoder[H :: T] =
  new PimEncoder[H :: T] {
    override def encode(a: H :: T): String = headEncoder.encode(a._1) + ":" + tailEncoder.encode(a._2)
  }


// summoner
def encode[A](a:A)(implicit encoder:PimEncoder[A]):String = encoder.encode(a)


import scala.reflect.runtime.universe._

show {
  reify {
    encode(tsss)
  }
}

encode(tsss)

encode(tsss)(
  hlistEncoder(
    intPimEncoder, hlistEncoder(
      strPimEncoder, hlistEncoder(
        boolPimEncoder, hnilPimEncoder)
    )
  )
)

// case class <=> HList

def tuple2hlist[A,B](t: (A,B)): A :: B :: HNil = (t._1, (t._2, HNil))
def tuple2hlist[A,B,C](t: (A,B,C)): A :: B :: C :: HNil = (t._1, (t._2, (t._3, HNil)))
// up to 22

val t3 = (19, "api.pimpay.ru", false)
val t2 = (5,10)


val ss = StatusSignal(20, "db", true)

encode { tuple2hlist( StatusSignal.unapply(ss).get ) }

// homework
def length[A](hlist: A):Int = {
  var acc = 0

  def len[A](list: A):Int = list match {
    case HNil => acc;
    case (_,t) => acc = acc + 1; len(t)
  }

  len(hlist)
}
length( tuple2hlist( (1,2,3) )) == 3
length( tuple2hlist( (2,3) )) == 2
length(tuple2hlist( (1,2,3) ))