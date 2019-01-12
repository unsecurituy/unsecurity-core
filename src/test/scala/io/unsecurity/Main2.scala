package io.unsecurity
import cats.effect.{ExitCode, IO, IOApp}
import io.unsecurity.Main2.unsecurity2
import io.unsecurity.Unsecure.{Endpoint, PathMatcher}
import io.unsecurity.hlinx.HLinx.{Root, SimpleLinx}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, Plan}
import org.http4s.{HttpRoutes, Method, Response}

import scala.concurrent.ExecutionContext.Implicits.global

object Main2 extends IOApp {
  val unsecurity2: Unsecurity2[IO] = new Unsecurity2[IO] {}
  import unsecurity2._

  val server: Server[IO] = Server[IO](
    port = 8088,
    host = "0.0.0.0"
  )

  val auth = new unsecurity2.UnsecureAuthenticator

  import auth._

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

  def toHttpRoutes(endpoints: List[Complete]): HttpRoutes[IO] = {
    val linxesToList: Map[List[SimpleLinx], List[Complete]] = endpoints.groupBy(_.key)

    val mergedRoutes: List[unsecurity2.Complete] = for {
      (_, groupedEndpoints) <- linxesToList.toList
    } yield {
      groupedEndpoints.reduce(_ merge _)
    }

    val compiledRoutes : List[PathMatcher[IO, Response[IO]]] =
      mergedRoutes.map(_.compile)


    val reducedRoutes: PathMatcher[IO, Response[IO]] = compiledRoutes.reduce(_ orElse _)

    val PathMapping = Plan[IO]().PathMapping

    val service: HttpRoutes[IO] = HttpRoutes.of[IO](
      PathMapping(reducedRoutes)
    )

    service
  }

  override def run(args: List[String]): IO[ExitCode] = {
    import cats.implicits._

    // mål: serve en enkelt rute
    // sjekke:
    // - not found
    // - method not allowed

    val httpRoutes = toHttpRoutes(List(helloWorld))

    server
      .serve(httpRoutes)
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
