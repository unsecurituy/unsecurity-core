package io.unsecurity

import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Response}

abstract class Unsecurity2[F[_]: Sync] {

  def toHttpRoutes(endpoints: List[Complete]): HttpRoutes[F]

  object Read {
    def Nothing: EntityDecoder[F, Unit] =
      implicitly[EntityDecoder[F, Unit]]

    def json[A: Decoder]: EntityDecoder[F, A] =
      org.http4s.circe.jsonOf[F, A]

    def raw: EntityDecoder[F, String] =
      implicitly[EntityDecoder[F, String]]
  }

  object Write {
    def Nothing: EntityEncoder[F, Unit] =
      implicitly[EntityEncoder[F, Unit]]

    def json[A: Encoder]: EntityEncoder[F, A] =
      org.http4s.circe.jsonEncoderOf[F, A]

    def raw: EntityEncoder[F, String] =
      implicitly[EntityEncoder[F, String]]
  }

  trait Secured[C, W] extends Completable[C, W] {
    def authorization(predicate: C => Boolean): Completable[C, W]
    override def resolve[C2](f: C => F[C2]): Secured[C2, W]
  }

  trait Authenticator[A] {
    def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Secured[(P, R, A), W]
    def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W]
  }


  trait Completable[C, W] {
    def run(f: C => Directive[F, W]): Complete
    def resolve[C2](f: C => F[C2]): Completable[C2, W]
  }


  trait Complete {
    def key: List[SimpleLinx]
    def merge(other: Complete): Complete
    def methodMap: Map[Method, Any => ResponseDirective[F]]
    def compile: PathMatcher[F, Response[F]]

    def ||(next: Complete): Complete
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

