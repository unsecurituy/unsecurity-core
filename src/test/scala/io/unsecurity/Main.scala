package io.unsecurity

import cats.effect._
import io.unsecurity.Test.MyAuthenticatedUser
import cats.implicits._
import io.unsecurity.Unsecured.{Route, UnsafeGetRoute, UnsafePostRoute}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.{HttpRoutes, Method, Response, Status}
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.server.blaze._

object Main extends IOApp {

  import io.unsecurity.hlinx.HLinx._

  import scala.concurrent.ExecutionContext.Implicits.global

  val unsecurity: Unsecurity[IO, MyAuthenticatedUser] = Unsecurity[IO, MyAuthenticatedUser]

  val server: Serve[IO] = Serve[IO](port = 8088, host = "0.0.0.0")

  val helloWorld: UnsafeGetRoute[IO, HNil] =
    unsecurity
      .unsecured(route = Root / "hello")
      .produces[String]
      .GET { _ =>
        Directive.success("Hello world!")
      }

  val helloName: UnsafeGetRoute[IO, String ::: HNil] =
    unsecurity.unsecured(route = Root / "hello" / param[String]("name"))
      .produces[String]
      .GET {
        case (name ::: HNil) =>
          Directive.success(s"Hello, $name!")
      }

  val postRoute: UnsafePostRoute[IO, String, String, String ::: HNil] =
    unsecurity.unsecured(route = Root / "hello" / param[String]("name"))
      .produces[String]
      .consumes[String]
      .POST { case (body, (name ::: HNil)) => Directive.success(s"hello, ${name}. This is $body") }

  val routes: List[Route[IO]] = List(helloWorld, helloName, postRoute)

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    val mergedRoutes: List[PartialFunction[String, ResponseDirective[IO]]] =
      routes.groupBy(_.key).mapValues(rs => rs.map(_.compile).reduce(_ merge _)).values.toList.map(_.compile)

    val reducedRoutes = mergedRoutes.reduce(_ orElse _)

    server.stream(reducedRoutes).compile.drain.as(ExitCode.Success)
  }
}
