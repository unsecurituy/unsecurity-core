package io.unsecurity

import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, Plan}
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Response}

abstract class AbstractUnsecurity2[F[_]: Sync, U] {

  case class Endpoint[P <: HLinx.HList, R, W](method: Method,
                                              path: HLinx[P],
                                              produces: EntityEncoder[F, W] = Produces.Nothing,
                                              accepts: EntityDecoder[F, R] = Accepts.EmptyBody
                                             )

  type PathMatcher[F[_], A] = PartialFunction[String, Directive[F, A]]


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
    def resolve[C2](f: C => C2): Completable[C2, W]
  }

  trait Secured[C, W] extends Completable[C, W] {
    def authorization(predicate: C => Boolean): Completable[C, W]
    override def resolve[C2](f: C => C2): Secured[C2, W]
  }

  trait Complete {
    def key: List[SimpleLinx]
    def merge(other: AbstractUnsecurity2[F,U]#Complete): AbstractUnsecurity2[F,U]#Complete
    def methodMap: Map[Method, Any => ResponseDirective[F]]
    def compile: PathMatcher[F, Response[F]]
  }

  def toHttpRoutes(endpoints: List[AbstractUnsecurity2[F,U]#Complete]): HttpRoutes[F] = {
    val linxesToList: Map[List[SimpleLinx], List[AbstractUnsecurity2[F,U]#Complete]] = endpoints.groupBy(_.key)

    val mergedRoutes: List[AbstractUnsecurity2[F,U]#Complete] =
      linxesToList.toList.map {
        case (_, groupedEndpoints) => groupedEndpoints.reduce(_ merge _)
      }

    val compiledRoutes: List[PathMatcher[F, Response[F]]] =
      mergedRoutes.map(_.compile)

    val reducedRoutes: PathMatcher[F, Response[F]] = compiledRoutes.reduce(_ orElse _)

    val PathMapping = Plan[F]().PathMapping

    val service: HttpRoutes[F] = HttpRoutes.of[F](
      PathMapping(reducedRoutes)
    )

    service
  }
}
