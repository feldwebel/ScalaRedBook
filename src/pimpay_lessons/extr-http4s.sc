object -> {
  def unapply(s:String):Option[(HttpMethod, Path)] = Some(GET, new Path {})
}

object / {
  def unapply(arg: Path): Option[(Path,String)] = Some(new Path {} -> "view")
}

trait HttpMethod
trait Path
case object GET extends HttpMethod
case class Url(method:HttpMethod,p:Path)

case object Root extends Path


trait Action[A] {}

object Router {
  def apply(s:String => Action[String]):Action[String] = new Action[String] {}
}

object Ok {
  def apply(s:String):Action[String] = new Action[String] {}
}

val router = Router {
  case GET -> Root / "view" => Ok("Hello, better world.")
}