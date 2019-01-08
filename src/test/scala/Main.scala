import cats.effect._
import no.scalabin.http4s.directives.{Directive, DirectiveOps, RequestDirectives}
import io.unsecurity.Test.MyAuthenticatedUser
import io.unsecurity._
import cats.effect.IO
import fs2.Stream
import io.unsecurity.hlinx.HLinx.HList
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import org.http4s.{Response, Status}

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  import io.unsecurity.hlinx.HLinx._

  val unsecurity = Unsecurity[IO, MyAuthenticatedUser]

  val route: CompleteRoute[IO, Nothing, Nothing, String, HNil] =
    unsecurity.unsafe
      .route(Root / "hello")
      .produces[String]
      .GET { params =>
        Directive.success("ok")
      }

  val route2: CompleteRoute[IO, Nothing, Nothing, String, String ::: HNil] =
    unsecurity.unsafe
      .route(Root / "hello" / param[String]("name"))
      .produces[String]
      .GET { params =>
        val name = params.tupled
        Directive.success(s"hello, ${name}")
      }

  val l: List[CompleteRoute[IO, Nothing, Nothing, String, _ <: HList]] = List(route, route2)

  val r: List[PartialFunction[String, ResponseDirective[IO]]] = l.map(r => Compiler.compile(r))

  override def run(args: List[String]): IO[ExitCode] =
    ???
}

case class Serve[F[_]](port: Int, host: String)(implicit eff: ConcurrentEffect[F], cs: ContextShift[F], timer: Timer[F], globalEC: ExecutionContext)
    extends DirectiveOps[F] {

//  def stream(routes: List[CompleteRoute[]]*): Stream[F, ExitCode] = {
//    ???
//  }
}

object Compiler {
  def compile[F[_], USER <: AuthenticatedUser[_, _], IN, OUT, PathParams <: HList](
      completeRoute: CompleteRoute[F, USER, IN, OUT, PathParams]): PartialFunction[String, ResponseDirective[F]] = {

    object Capture {
      def unapply(path: String): Option[Either[String, PathParams]] =
        completeRoute.route.capture(path)
    }

    {
      case Capture(params) =>
        params match {
          case Left(errorMessage) =>
            ???
          case Right(pathParams) =>
            for {
              req <- Directive.request
//              res <- completeRoute.f(null, null, pathParams)
            } yield {
              ???
//              res
            }

//            Directive.success()
            ???
        }
    }
  }
}
