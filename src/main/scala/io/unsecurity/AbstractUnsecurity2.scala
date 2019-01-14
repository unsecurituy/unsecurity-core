package io.unsecurity

import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Response}

abstract class AbstractUnsecurity2[F[_]: Sync, U] {

  def toHttpRoutes(endpoints: List[Complete]): HttpRoutes[F] //TODO: Legg denne i Serve elns, bare ikke her :)

  case class Endpoint[P <: HLinx.HList, R, W](method: Method,
                                              path: HLinx[P],
                                              accepts: EntityDecoder[F, R] = Accepts.EmptyBody, //requestType
                                              produces: EntityEncoder[F, W] = Produces.Nothing) //responseType

  def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Secured[(P, R, U), W]
  def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W]

  object Accepts {
    def EmptyBody: EntityDecoder[F, Unit] =
      implicitly[EntityDecoder[F, Unit]]

    def json[R: Decoder]: EntityDecoder[F, R] =
      org.http4s.circe.jsonOf[F, R]

    def raw: EntityDecoder[F, String] =
      implicitly[EntityDecoder[F, String]]
  }

  object Produces {
    def Nothing: EntityEncoder[F, Unit] =
      implicitly[EntityEncoder[F, Unit]]

    def json[W: Encoder]: EntityEncoder[F, W] =
      org.http4s.circe.jsonEncoderOf[F, W]

    def raw: EntityEncoder[F, String] =
      implicitly[EntityEncoder[F, String]]
  }

  trait Completable[C, W] {
    def run(f: C => Directive[F, W]): Complete
    def resolve[C2](f: C => F[C2]): Completable[C2, W]
  }

  trait Secured[C, W] extends Completable[C, W] {
    def authorization(predicate: C => Boolean): Completable[C, W]
    override def resolve[C2](f: C => F[C2]): Secured[C2, W]
  }

  trait Complete {
    def key: List[SimpleLinx]
    def merge(other: Complete): Complete
    def methodMap: Map[Method, Any => ResponseDirective[F]]
    def compile: PathMatcher[F, Response[F]]
  }
}
