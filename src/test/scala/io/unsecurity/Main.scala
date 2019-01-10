package io.unsecurity

import cats.effect._
import io.unsecurity.Test.MyAuthenticatedUser
import cats.implicits._
import no.scalabin.http4s.directives.Directive
import org.http4s.{HttpRoutes, Response, Status}
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.server.blaze._


object Main extends IOApp {

  import io.unsecurity.hlinx.HLinx._

  import scala.concurrent.ExecutionContext.Implicits.global

  val unsecurity: Unsecurity[IO, MyAuthenticatedUser] = Unsecurity[IO, MyAuthenticatedUser]

  val server: Serve[IO] = Serve[IO](port = 8088, host = "0.0.0.0")

  val helloWorld: UnsafeGetRoute[IO, HNil] =
    unsecurity.unsafe
      .route(Root / "hello")
      .produces[String]
      .GET { _ =>
        Directive.success("Hello world!")
      }

  val helloName: UnsafeGetRoute[IO, String ::: HNil] =
    unsecurity.unsafe
      .route(Root / "hello" / param[String]("name"))
      .produces[String]
      .GET { case (name ::: HNil) =>
        Directive.success(s"Hello, $name!")
      }

  /*
    val route2: UnsafePostRoute[IO, String, String ::: HNil] =
      unsecurity.unsafe
        .route(Root / "hello" / param[String]("name"))
        .produces[String]
        .consumes[String]
        .POST { (body, params) =>
          val name = params.tupled
          Directive.success(s"hello, ${name}")
        }
  */

  val routes: List[Routable[IO]] = List(helloWorld, helloName)

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    server.stream(routes.map(_.toRoute).reduce(_ orElse _)).compile.drain.as(ExitCode.Success)
  }
}
