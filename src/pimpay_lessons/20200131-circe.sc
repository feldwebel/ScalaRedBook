import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._

case class Point(x:Int, y:Int)
case class Plot(plot:Vector[Point])


val plot = Plot {
  (for { i <- 1 to 5; j <- 1 to 5 } yield Point(i, j)).toVector
}

plot

implicit val pointEncoder:Encoder[Point] = (p:Point) => (p.x -> p.y).asJson

val rawJson = plot.asJson.noSpaces


implicit val pointDecoder:Decoder[Point] = (h:HCursor) => for {
  x <- h.downN(0).as[Int]
  y <- h.downN(1).as[Int]







} yield Point(x, y)
decode[Plot](rawJson)