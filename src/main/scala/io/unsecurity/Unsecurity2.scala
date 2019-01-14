package io.unsecurity

import cats.Applicative
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.unsecurity.Unsecure.PathMatcher
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, RequestDirectives}
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, Method, Response, Status}

abstract class Unsecurity2[F[_]: Sync: Applicative] {

  object Read {
    def Nothing: EntityDecoder[F, Unit] =
      implicitly[EntityDecoder[F, Unit]]

    def json[A: Decoder]: EntityDecoder[F, A] =
      org.http4s.circe.jsonOf[F, A]
  }

  object Write {
    def Nothing: EntityEncoder[F, Unit] =
      implicitly[EntityEncoder[F, Unit]]

    def json[A: Encoder]: EntityEncoder[F, A] =
      org.http4s.circe.jsonEncoderOf[F, A]
  }

  trait Safe[C, W] extends Completable[C, W] {
    def authorization(predicate: C => Boolean): Completable[C, W]
    override def resolve[C2](f: C => F[C2]): Safe[C2, W]
  }

  trait Authenticator[A] {
    def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Safe[(P, R, A), W]
    def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W] =
      ???
  }

  class UnsecureAuthenticator extends Authenticator[Unit] with RequestDirectives[F] {
    override def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Safe[(P, R, Unit), W] = ???
    override def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W] = {
      MyCompletable[(P, R), W](
        key = endpoint.path.toSimple.reverse,
        pathMatcher = Unsecure.createPathMatcher[F, P](endpoint.path).asInstanceOf[PathMatcher[F, Any]],
        methodMap = Map(
          endpoint.method -> { pp: P =>
            implicit val entityDecoder: EntityDecoder[F, R] = endpoint.read
            for {
              r <- request.bodyAs[F, R]
            } yield {
              (pp, r)
            }
          }.asInstanceOf[Any => Directive[F, (P, R)]]
        ),
        entityEncoder = endpoint.write
      )
    }
  }

  trait Completable[C, W] {
    def run(f: C => Directive[F, W]): Complete
    def resolve[C2](f: C => F[C2]): Completable[C2, W]
  }

  case class MyCompletable[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Completable[C, W] {
    override def run(f: C => Directive[F, W]): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues { a2dc: (Any => Directive[F, C]) =>
          a2dc.andThen { dc =>
            for {
              c <- dc
              w <- f(c)
            } yield {
              Response[F]()
                .withEntity(w)(entityEncoder)
            }
          }
        }
      )
    }

    override def resolve[C2](f: C => F[C2]): Completable[C2, W] = ???
  }

  trait Complete {
    def key: List[SimpleLinx]
    def merge(other: Complete): Complete
    def ||(next: Complete): Complete
    def methodMap: Map[Method, Any => ResponseDirective[F]]
    def compile: PathMatcher[F, Response[F]]
  }

  case class MyComplete(
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => ResponseDirective[F]]
  ) extends Complete {
    override def ||(next: Complete): Complete = ???
    override def merge(other: Complete): Complete = {
      this.copy(
        methodMap = this.methodMap ++ other.methodMap
      )
    }
    override def compile: PathMatcher[F, Response[F]] = {
      def allow(methods: List[Method]): Allow = Allow(methods.head, methods.tail: _*)

      pathMatcher.andThen { pathParamsDir =>
        for {
          req        <- Directive.request
          pathParams <- pathParamsDir
          res <- if (methodMap.isDefinedAt(req.method)) methodMap(req.method)(pathParams)
                else Directive.error(Response[F](Status.MethodNotAllowed).putHeaders(allow(methodMap.keySet.toList)))
        } yield {
          res
        }
      }
    }
  }

  case class Endpoint[P <: HLinx.HList, R, W](method: Method,
                                              path: HLinx[P],
                                              read: EntityDecoder[F, R] = Read.Nothing,
                                              write: EntityEncoder[F, W] = Write.Nothing)

  /*
  val auth: Authenticator[IO, String] = ???

  import auth._

  val aRoute =
    unsecure(
      Endpoint(
        method = Method.POST,
        path = Root / "a"
      )
    )

  val bRoute = secure(
    Endpoint(
      method = Method.GET,
      path = Root / "b" / param[Int]("b"),
      read = Read.json[String],
      write = Write.json[Int]
    ))
    .authorization(_ => true)
    .run(_ => Directive.success(42))
 */

}
