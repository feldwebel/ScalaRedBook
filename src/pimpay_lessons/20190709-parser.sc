import scala.language.higherKinds
import scala.util.matching.Regex


trait Parsers[Parser[+_], ParseError] { self=>
  def string(s: String): Parser[String]

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def map[A, B](pa:Parser[A])(f: A => B): Parser[B] = ???
  def or[A](pa: Parser[A], pb: => Parser[A]): Parser[A] = ???
  def product[A, B](pa:Parser[A], pb:Parser[B]):Parser[(A, B)] = ???
  def regex(r:Regex): Parser[String] = ???
  def run[A](p:Parser[A])(input:String):Either[ParseError, A]

  //derivatives
  def char(c:Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def combine[A, B, C](pa:Parser[A], pb:Parser[B])(f: (A, B) => C): Parser[C] =
    product(pa, pb) map f.tupled
  def zeroOrMore[A](p:Parser[A]): Parser[List[A]] =
    combine(p, zeroOrMore(p))(_ :: _) or succeed(List.empty[A])
  def oneOrMore[A](p:Parser[A]): Parser[List[A]] =

  def productL[A, B](pa: Parser[A], pb: Parser[B]): Parser[A] = ???
  def productR[A, B](pa: Parser[A], pb: Parser[B]): Parser[B] = ???
  def digits:Parser[Int] = regex("\\d+".r) map (_.toInt)

  //syntax
  implicit class ParserSyntax[A](pa:Parser[A]) {
    def map[B](f:A => B): Parser[B] = self.map(pa)(f)
    def or(pb:Parser[A]): Parser[A] = self.or(pa, pb)
    def |(pb:Parser[A]): Parser[A] = self.or(pa, pb)
    def **[B](pb: Parser[B]): Parser[(A, B)] = product(pa, pb)
    def <*[B](pb: Parser[B]): Parser[A] = productL(pa, pb)
    def *>[B](pb: Parser[B]): Parser[B] = productR(pa, pb)
  }
}

sealed class JSON
case object JsNull extends JSON
case class JsBoolean[b: Boolean] extends JSON
def JSONParser[Parser[+_], ParseError](p:Parsers[Parser, ParseError]): Parser[JSON] =???