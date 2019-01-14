package io.unsecurity

import cats.effect._
import io.unsecurity.Test.MyAuthenticatedUser
import io.unsecurity.Unsecure.{Endpoints, UnsecureGetEndpoint, UnsecurePostEndpointRW}
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Directive

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends IOApp {
  val unsecurity: Unsecurity[IO, MyAuthenticatedUser] = Unsecurity[IO, MyAuthenticatedUser]

  val server: Server[IO] = Server[IO](
    port = 8088,
    host = "0.0.0.0"
  )

  val helloWorld: UnsecureGetEndpoint[IO, HNil] =
    unsecurity
      .unsecuredRoute(Root / "hello")
      .producesJson[String]
      .GET { _ =>
        Directive.success("Hello world!")
      }

  val helloName: UnsecureGetEndpoint[IO, String ::: HNil] =
    unsecurity
      .unsecuredRoute(Root / "hello" / param[String]("name"))
      .producesJson[String]
      .GET {
        case (name ::: HNil) =>
          Directive.success(s"Hello, $name!")
      }

  val postRoute: UnsecurePostEndpointRW[IO, String ::: HNil, String, String] =
    unsecurity
      .unsecuredRoute(Root / "hello" / param[String]("name"))
      .producesJson[String]
      .consumesJson[String]
      .POST { case (body, (name ::: HNil)) => Directive.success(s"hello, ${name}. This is $body") }

  val dummy = unsecurity
    .unsecuredRoute(Root / "fjon" / param[Int]("max"))
    .producesJson[String]
    .GET { p =>
      val max ::: HNil = p

      val range = Range(1, max).toList

      Directive.success(range.mkString(", "))
    }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    server
      .serve(
        new Endpoints[IO]
          .addRoute(helloWorld)
          .addRoute(helloName)
          .addRoute(helloName)
          .addRoute(postRoute)
          .toHttpRoutes)
      .compile
      .drain
      .as(ExitCode.Success)
  }

  // GET /a/b/p[String]("s")?q=fisk => /a/b/[variable]
  //                                   Set(q[Option[String]])
  // GET /a/b/p[Int]("i")?q=fisk => /a/b/[variable]
  // GET /a/b
}
