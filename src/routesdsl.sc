//http4s
//cats | scalaz
//fs2/monix/akka-stream
//cats-effect/scalazIO

import scala.reflect.runtime.universe._

/*val x = 2
x match {
  case 2 => "yahoo"
}

show (reify (
Server {
  case 2 => "yahoo"
  case 3 => "3"
  case _ => "default"
} (77)
))
//Server.run(2)

object Server {
  def apply(fn: Int => String): Int => String = fn
}*/

//type Request = String
//type Response = String
sealed trait Method

case object GET extends Method
case object POST extends Method
case class Request(method: Method, uri: String)

class Response(val code: Int, val body:String)
case class OK(override val body: String) extends Response(200, body)

case class Server(handler: PartialFunction[Request, Response]) {
  def handle(req: Request):Response =
    if (handler.isDefinedAt(req)) handler(req) else new Response(404, "Not Found")
}
val server = Server {
  case GET -> "/hello" => OK("Hello, buddy!")
}

object -> {
  def unapply(req: Request): Option[(Method, String)] = Some((req.method, req.uri))
}

server.handle(Request(GET, "/hello"))


/*val server = Server {
  case "/hello" => "Hello"
  case "/hello/bob" => "Hello, Bob"
  case _ => "Ooops 404"
}*/

/*server.handle("dfdfdfd")
server.handle("/hello")
server.handle("/hello/bob")*/

class Point (val x: Int, val y: Int)

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
  def unapply(arg: Point): Option[(Int, Int)] = Some((arg.x, arg.y))
}

object % {
  def unapply(arg: Point): Option[(Int, Int)] = Some((arg.x, arg.y))
}

val p = Point(2, 6)

p match {
  case Point(x, y @ isEvenPredicat()) => x -> y
}

object isEvenPredicat {
  def unapply(i: Int): Boolean = i % 2 == 0
}

object isEven {
  def unapply(i: Int): Option[Int] = if (i % 2 == 0) Some(i) else None
}

object isPositive {
  def unapply(i: Int): Option[Int] = if (i > 0) Some(i) else None
}

object Sqrt {
  def unapply(i: Int): Option[Double] = Some(Math.sqrt(i))
}

p match {
  case Point(x, isEven(isPositive(y))) => x -> y
}

p match {
  case Point(x, isEven(isPositive(Sqrt(y)))) => x -> y
}

val g = "     fdfdfdf  "

g match {
  case Trim(UpperCase(r)) => r
}

object Trim {
  def unapply(s: String): Option[String] = Some(s.trim)
}

object UpperCase {
  def unapply(s: String): Option[String] = Some(s.toUpperCase)
}

object Exploder {
  def unapplySeq(arg: String): Option[List[Char]] = Some(arg.toList)
}

"  abcde" match {
  case Trim(UpperCase(Exploder(first, _*))) => first // A
}

Point(2, 4) match {
  case x Point y => x -> y
}

Point(2, 4) match {
  case isEven(x) % y => x -> y
}