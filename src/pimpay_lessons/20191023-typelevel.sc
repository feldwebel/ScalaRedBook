import scala.annotation.implicitNotFound

sealed trait State

trait On extends State
trait Off extends State

case class Switch[S <: State]() {
  def on(implicit isOn: S Is Off) = Switch[On]() //Is[S, Off] S =:= Off
  def off(implicit isOn: S Is On) = Switch[Off]()
}

@implicitNotFound("Не смог доказать, что ${A} является ${B}")
trait Is[A,B]
implicit def is[A]:Is[A,A] = new Is[A,A]{}

Switch[On].off
Switch[On].off


case class Request(host:String, uri:String, query:Option[String] = None)

object Request {
  sealed trait Parts
  trait Host extends Parts
  trait Uri extends Parts
  trait Query extends Parts
  type Required  = Host with Uri


  protected case class Builder[P <: Parts](host:Option[String] = None,
                            uri:Option[String] = None,
                            query:Option[String] = None) {
    def withHost(host:String):Builder[P with Host]   = copy(host=Option(host))
    def withUri(uri:String):Builder[P with Uri]     = copy(uri=Option(uri))
    def withQuery(query:String):Builder[P with Query] = copy(query=Option(query))

    def build(implicit hasRequirements: P Requires Required):Request = Request(host.get, uri.get, query)
  }

  def builder:Builder[Parts] = Builder()
}

@implicitNotFound("Не смог доказать, что ${A} наследник ${B}")
trait Requires[A,B]
implicit def requires[A<:B, B]:Requires[A, B] = new Requires[A,B]{}

val req = Request.builder
  .withHost("pimpay.ru")
  .withUri("/hello")
  .withQuery("id=2")
  .build