import scala.util.Try

sealed trait Method
case object GET  extends Method
case object POST extends Method

case class Request(method:Method, uri:String)

class Response(val code:Int, val body:String) {
  override def toString = s"Response($code, $body)"
}
case class Ok(override val body:String) extends Response(200, body)


case class Server(handler: PartialFunction[Request, Response]) {
  def handle(req:Request):Response =
    if (handler.isDefinedAt(req)) handler(req) else new Response(404, "Not Found")
}

case object Root

val server = Server {
  case GET -> "user" / "profile" / IdVar(id) => Ok(s"Hello, user $id")
  /*case (GET|POST) -> Parts("user", action @ ("profile"|"view"), IdVar(id))
    if id != 0
  => Ok(s"Hello, $id! Action was $action")*/
}

object / {
  def unapply(s: String): Option[(String, String)] = {
    val chunks = s.split("/")
    if (chunks.length > 1) Some(chunks.init.mkString("/") -> chunks.last) else None
  }
}


object IdVar {
  def unapply(arg: String): Option[Int] = Try {arg.toInt} toOption
}

object Parts {
  def unapplySeq(arg: String): Option[Seq[String]] = Some(arg.split('/'))
}

object -> {
  def unapply(req:Request):Option[(Method, String)] = Some(req.method -> req.uri)
}

server.handle(Request(GET, "user/profile/1234"))