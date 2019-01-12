package io.unsecurity
import cats.effect.{ExitCode, IO, IOApp}
import io.unsecurity.Unsecurity2._
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx.Root
import no.scalabin.http4s.directives.Directive
import org.http4s.Method

import scala.concurrent.ExecutionContext.Implicits.global

object Main2 extends IOApp {
  import Unsecurity2._

  val server: Server[IO] = Server[IO](
    port = 8088,
    host = "0.0.0.0"
  )

  val auth = new UnsecureAuthenticator[IO]

  import auth._

  val helloWorld: Unsecurity2.Complete[IO] =
    unsecure(
      Endpoint(
        method = Method.GET,
        path = Root / "hello",
        write = Write.json[String]
      )
    ).run { _ =>
      Directive.success("Hello world")
    }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    // mål: serve en enkelt rute
    // sjekke:
    // - not found
    // - method not allowed
/*
    server
      .serve(httpRoutes)
      .compile
      .drain
      .as(ExitCode.Success)
*/
    ???
  }
}
