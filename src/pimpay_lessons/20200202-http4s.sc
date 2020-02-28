import cats._
import cats.effect.{ExitCode, IO, IOApp}
//import cats.effect._
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object Main extends IOApp {

  val helloWorldService = HttpRoutes.of[IO] {
    case GET -> Root / "db"  => { Thread.sleep(4000); Ok("Db is ok!") }
    case GET -> Root / "api" => { Thread.sleep(4000); Ok("Api is ok") }

    case GET -> Root / "health" => {
      // ТАК НЕЛЬЗЯ:
      val dbF  = Future { scala.io.Source.fromURL(new java.net.URL("http://localhost:8080/db"), "UTF-8").mkString }
      val apiF = Future { scala.io.Source.fromURL(new java.net.URL("http://localhost:8080/api"), "UTF-8").mkString }

      val res = for {
        db  <- dbF
        api <- apiF
      } yield s"db: $db, api: $api"

      val txt = Await.result(res, Duration.Inf)
      Ok(s"$txt")
    }
  }.orNotFound

  def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO]
      .bindHttp(8080, "localhost")
      .withHttpApp(helloWorldService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
// defined object Main