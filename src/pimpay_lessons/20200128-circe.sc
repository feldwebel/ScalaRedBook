import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._


sealed trait i18n_weight_format
case object Kg extends i18n_weight_format
case object Lbs extends i18n_weight_format

class Custom(weight:Double) {
  def getWeight: Double = weight
}

implicit def customerEncoder(implicit format:i18n_weight_format):Encoder[Custom] =
  (c:Custom) => (format match {
    case Kg => c.getWeight + "kg"
    case Lbs => c.getWeight * 2.25 + "lbs"
  }).asJson

case class Person(name: String, weight: Custom)
val tom = Person("tom", new Custom(80.0))

{
  implicit val _:i18n_weight_format = Lbs
  tom.asJson
}

{
  implicit val _:i18n_weight_format = Kg
  tom.asJson
}
