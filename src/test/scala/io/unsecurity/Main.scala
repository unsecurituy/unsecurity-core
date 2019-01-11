package io.unsecurity

import cats.effect._
import io.unsecurity.Test.MyAuthenticatedUser
import io.unsecurity.Unsecured.{Route, UnsecuredGetRoute, UnsafePostRoute}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import io.unsecurity.hlinx.HLinx._
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends IOApp {
  val unsecurity: Unsecurity[IO, MyAuthenticatedUser] = Unsecurity[IO, MyAuthenticatedUser]

  val server: Serve[IO] = Serve[IO](port = 8088, host = "0.0.0.0")

  val helloWorld: UnsecuredGetRoute[IO, HNil] =
    unsecurity
      .unsecuredRoute(Root / "hello")
      .producesJson[String]
      .GET { _ =>
        Directive.success("Hello world!")
      }

  val helloName: UnsecuredGetRoute[IO, String ::: HNil] =
    unsecurity.unsecuredRoute(Root / "hello" / param[String]("name"))
      .producesJson[String]
      .GET {
        case (name ::: HNil) =>
          Directive.success(s"Hello, $name!")
      }

  val postRoute: UnsafePostRoute[IO, String ::: HNil, String, String] =
    unsecurity.unsecuredRoute(Root / "hello" / param[String]("name"))
      .producesJson[String]
      .consumesJson[String]
      .POST { case (body, (name ::: HNil)) => Directive.success(s"hello, ${name}. This is $body") }

  val dummy = unsecurity
    .unsecuredRoute(Root / "fjon" / param[Int]("antall"))
    .producesJson[String]
    .GET {
      p =>
        val antall ::: HNil = p
        Directive.success(s"$antall")
    }

  val routes: List[Route[IO]] = List(helloWorld, helloName, postRoute)

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    val mergedRoutes: List[PartialFunction[String, ResponseDirective[IO]]] =
      routes.groupBy(_.key).mapValues(rs => rs.map(_.compile).reduce(_ merge _)).values.toList.map(_.compile)

    val reducedRoutes = mergedRoutes.reduce(_ orElse _)

    server.stream(reducedRoutes).compile.drain.as(ExitCode.Success)
  }
}
