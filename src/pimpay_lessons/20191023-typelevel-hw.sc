import scala.annotation.implicitNotFound

sealed trait Language

object PHP extends Language
object Scala extends Language

implicit class languageOps[L <: Language](l:L) {
  def sucks(implicit sucks: Sucks[L]): Boolean = true
}

@implicitNotFound("Ой не гони, php огонь")
trait Sucks[L <: Language]

Scala sucks // should compile
// обязательно new line

PHP sucks // shouldn't compile

