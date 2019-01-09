package io.unsecurity

import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Timer}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Plan
import org.http4s.HttpRoutes
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext

case class Serve[F[_]](port: Int, host: String)
                      (implicit eff: ConcurrentEffect[F],
                       cs: ContextShift[F],
                       timer: Timer[F],
                       globalEC: ExecutionContext) {

  def stream(routes: PartialFunction[String, ResponseDirective[F]]*): fs2.Stream[F, ExitCode] = {
    val PathMapping = Plan[F]().PathMapping

    val service = HttpRoutes.of[F](
      PathMapping(
        routes.toList.reduce[PartialFunction[String, ResponseDirective[F]]](_ orElse _)
      )
    )
    import org.http4s.implicits._

    val httpApp = Router("/" -> service).orNotFound

    for {
      _ <- BlazeServerBuilder[F]
        .bindHttp(port, host)
        .enableHttp2(false)
        .withWebSockets(false)
        .withExecutionContext(globalEC)
        .withNio2(true)
        .withConnectorPoolSize(4)
        .withHttpApp(httpApp)
        .serve
    } yield ExitCode.Success
  }
}
