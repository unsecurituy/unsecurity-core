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

  val unsecurity = Unsecurity[IO, MyAuthenticatedUser]

  val server: Serve[IO] = Serve[IO](port = 8088, host = "0.0.0.0")

  val route: UnsafeGetRoute[IO, String ::: HNil] =
    unsecurity.unsafe
      .route(Root / "hello" / param[String]("name"))
      .produces[String]
      .GET { params =>
        Directive.success("ok")
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

/*
  val l: List[GroupableRoute] = List(route, route2)

  private val linxesToRoutes: Map[List[SimpleLinx], List[GroupableRoute]] = l.groupBy(_.key)

  println(linxesToRoutes)
*/

  override def run(args: List[String]): IO[ExitCode] = {
    val pf = new PartialFunction[String, Directive[IO, Response[IO]]] {
      override def isDefinedAt(x: String): Boolean = route.route.capture(x).isDefined

      override def apply(v1: String): Directive[IO, Response[IO]] = {
        val value: Either[String, String ::: HNil] = route.route.capture(v1).get

        value match {
          case Left(errorMsg) =>
            Directive.error(
              Response[IO](Status.BadRequest)
                .withEntity(errorMsg)
            )

          case Right(params) =>
            route.f(params)

        }
      }
    }

    import cats.implicits._

    server.stream(pf).compile.drain.as(ExitCode.Success)
  }
}
