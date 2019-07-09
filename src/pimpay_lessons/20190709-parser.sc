import scala.language.higherKinds
import scala.util.matching.Regex
import scala.util.parsing.json.JSON

// HKT

trait Parsers[Parser[+_], ParseError] { self =>
  // primitives
  def string(s:String):Parser[String] = ???
  def succeed[A](a:A):Parser[A] = string("") map {_ => a}
  def map[A,B](pa:Parser[A])(f: A => B):Parser[B] = ???
  def or[A](p1:Parser[A], p2: => Parser[A]):Parser[A] = ???
  def product[A,B](pa:Parser[A], pb:Parser[B]):Parser[(A,B)] = ???
  def regex(r:Regex):Parser[String] = ???
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  // derivatives
  def char(c: Char): Parser[Char] = string(c.toString) map { _.charAt(0) }
  def combine[A,B,C](pa:Parser[A],pb:Parser[B])(f: (A,B) => C):Parser[C] = product(pa,pb) map f.tupled
  def zeroOrMore[A](p:Parser[A]):Parser[List[A]] = combine(p,zeroOrMore(p))(_ :: _) | succeed(List.empty[A])
  def oneOrMore[A](p:Parser[A]):Parser[List[A]] = ???
  def productL[A,B](pa:Parser[A],pb:Parser[B]):Parser[A] = product(pa,pb) map {_._1}
  def productR[A,B](pa:Parser[A],pb:Parser[B]):Parser[B] = product(pa,pb) map {_._2}

  // helpers
  def digits:Parser[Int] = regex("\\d+".r) map (_.toInt)
  def whitespace:Parser[String] = regex("\\s+".r)


  // syntax
  implicit class ParserSyntax[A](pa:Parser[A]) {
    def map[B](f:A => B):Parser[B] = self.map(pa)(f)
    def or(pb:Parser[A]) = self.or(pa,pb)
    def |(pb:Parser[A])  = self.or(pa,pb)
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
case class JsArray(vals:Array[JSON]) extends JSON
case class JsObject(obj:Map[String,JSON]) extends JSON


def jsonParser[Parser[+_],ParseError](P:Parsers[Parser,ParseError]):Parser[JSON] = ???

//run(char('c'))("c") == Right('c')

// run(string("ab")|string("cc"))("cc") == Right("cc")
// run(string("ab")|string("cc"))("ab") == Right("ab")

// run(zeroOrMore('a'))("aaa") == Right(Seq('a','a','a')))
//run(string("abc"))("abc") == Right("abc")
//run(string("abc"))("abcd") == Left
//  parsed|exetucted|done
// ?<=[\d+]ab
// product(\d+)(ab) ._2


// run(product(string("aa"),string("bb"))("aabb") == Right("aabb")

//0\ {abc}

// "{" *> "abc" <* "}" // Parser["abc"]