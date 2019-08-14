import scala.language.higherKinds
import scala.util.matching.Regex
//import scala.util.parsing.json.JSON
import scala.language.postfixOps

// HKT

trait Parsers[Parser[+_], ParseError] { self =>
  // primitives
  def string(s:String):Parser[String]
  def succeed[A](a:A):Parser[A]
  def or[C, A <: C, B <: C](p1:Parser[A], p2: => Parser[B]):Parser[C]
  def regex(r:Regex):Parser[String]
  def flatMap[A,B](pa:Parser[A])(f: A => Parser[B]):Parser[B]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  // derivatives
  def map[A,B](pa:Parser[A])(f: A => B):Parser[B] = flatMap(pa)(f andThen succeed)
  def product[A,B](pa:Parser[A], pb:Parser[B]):Parser[(A,B)] = flatMap(pa)(a => map(pb)(b => (a,b)))
  def char(c: Char): Parser[Char] = string(c.toString) map { _.charAt(0) }
  def combine[A,B,C](pa:Parser[A],pb:Parser[B])(f: (A,B) => C):Parser[C] = product(pa,pb) map f.tupled
  def zeroOrMore[A](p:Parser[A]):Parser[List[A]] = combine(p,zeroOrMore(p))(_ :: _) | succeed(List.empty[A])
  def oneOrMore[A](p:Parser[A]):Parser[List[A]] = combine(p, zeroOrMore(p))(_ :: _)
  def productL[A,B](pa:Parser[A],pb:Parser[B]):Parser[A] = product(pa,pb) map {_._1}
  def productR[A,B](pa:Parser[A],pb:Parser[B]):Parser[B] = product(pa,pb) map {_._2}

  // helpers
  def digits:Parser[Int] = regex("\\d+".r) map (_.toInt)
  def whitespace:Parser[String] = regex("\\s+".r)


  // syntax
  implicit class ParserSyntax[A](pa:Parser[A]) {
    def map[B](f:A => B):Parser[B] = self.map(pa)(f)
    def or[C >: A, B <: C](pb:Parser[C]) = self.or(pa,pb)
    def |[C >: A, B <: C](pb:Parser[C])  = self.or(pa,pb)
    def **[B](pb:Parser[B]):Parser[(A,B)] = product(pa,pb)
    def <*[B](pb:Parser[B]):Parser[A] = productL(pa,pb)
    def *>[B](pb:Parser[B]):Parser[B] = productR(pa,pb)
  }
}


sealed trait JSON
case object JsNull extends JSON
case class JsBoolean(b:Boolean) extends JSON
case class JsNumber(n:Double) extends JSON
case class JsString(s:String) extends JSON
case class JsArray(vals:IndexedSeq[JSON]) extends JSON
case class JsObject(obj:Map[JsString,JSON]) extends JSON


def jsonParser[Parser[+_],ParseError](P:Parsers[Parser,ParseError]):Parser[JSON] = {
  import P._

  // helpers
  def wsp[A](pa:Parser[A]):Parser[A] = spaces *> pa <* spaces
  def ws(s:String):Parser[String] = wsp(string(s))

  implicit class StringOps(s:String) {
    def as[A](v:A):Parser[A] = ws(s) map (_ => v)
  }

  implicit class ParserOps[A](pa:Parser[A]) {
    def whitespaced:Parser[A] = wsp(pa)
  }

  def spaces = regex("\\s*".r)

  def wrapped[A](pa:Parser[A], l:String, r:String) = ws(l) *> pa <* ws(r)
  def csv[A](pa:Parser[A]):Parser[List[A]] = combine(pa, zeroOrMore(ws(",") *> pa)){ _ +: _ } | (spaces map {_ => List.empty[A]})

  // grammar
  def jsNull:Parser[JsNull.type] =  "null" as JsNull
  def jsBool:Parser[JsBoolean] = ("true" as JsBoolean(true)) | ("false" as JsBoolean(false))
  def jsNum:Parser[JsNumber] = regex("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r) map (s => JsNumber(s.toDouble)) whitespaced
  def jsStr:Parser[JsString] = regex(""""\w*"""".r) map JsString whitespaced // TODO: fix RE

  def jsLiteral:Parser[JSON] = jsBool | jsNull | jsNum | jsStr

  def jsArr:Parser[JsArray] = (wrapped(csv(json), "[", "]") whitespaced) map (l => JsArray(l.toIndexedSeq))

  def pair:Parser[(JsString,JSON)] = (jsStr <* ws(":")) ** json
  def jsObj:Parser[JsObject] = (wrapped(csv(pair), "{", "}") whitespaced) map (l => JsObject(l.toMap))

  // final
  def json:Parser[JSON] = jsObj | jsArr | jsLiteral

  json
}

trait MyParser[+A] {
}

type ParseError = String

object MyParsersEngine extends Parsers[MyParser,ParseError] {
  override def string(s: String): MyParser[String] = ???
  override def succeed[A](a: A): MyParser[A] = ???
  override def or[C, A <: C, B <: C](p1: MyParser[A], p2: => MyParser[B]): MyParser[C] = ???
  override def regex(r: Regex): MyParser[String] = ???
  override def flatMap[A, B](pa: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = ???
  override def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = ???
}


MyParsersEngine.run(jsonParser(MyParsersEngine))(""" { "key": "value", "key2": [1,2,3], "key3": null } """)