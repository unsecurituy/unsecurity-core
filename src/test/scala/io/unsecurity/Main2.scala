package io.unsecurity

import cats.effect.{ExitCode, IO, IOApp}
import io.unsecurity.hlinx.HLinx.Root
import no.scalabin.http4s.directives.Directive
import org.http4s.Method

import scala.concurrent.ExecutionContext.Implicits.global

object Main2 extends IOApp {
  val unsecurity2: MyUnsecurity2[IO, String] = new MyUnsecurity2[IO, String] {}
  import unsecurity2._

  val server: Server[IO] = Server[IO](
    port = 8088,
    host = "0.0.0.0"
  )


  val helloWorld: unsecurity2.Complete =
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

    val httpRoutes = toHttpRoutes(List(helloWorld))

    server
      .serve(httpRoutes)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
