package io.unsecurity

import cats.Applicative
import cats.effect.Sync
import io.circe.{Decoder, Encoder}
import io.unsecurity.Unsecure.PathMatcher
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, RequestDirectives}
import org.http4s.{EntityDecoder, EntityEncoder, Method}

abstract class Unsecurity2[F[_]: Sync: Applicative] {

  object Read {
    def No: EntityDecoder[F, Unit] =
      implicitly[EntityDecoder[F, Unit]]

    def json[A: Decoder]: EntityDecoder[F, A] =
      org.http4s.circe.jsonOf[F, A]
  }

  object Write {
    def No: EntityEncoder[F, Unit] =
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
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Completable[C, W] {
    override def run(f: C => Directive[F, W]): Complete = {

      ???
    }
    override def resolve[C2](f: C => F[C2]): Completable[C2, W] = ???
  }

  trait Complete {
    def ||(next: Complete): Complete
  }

  case class MyComplete(pathMatcher: PartialFunction[String, Any], methodMap: Map[Method, Any => ResponseDirective[F]])
      extends Complete {
    override def ||(next: Complete): Complete =
      ???
  }

  case class Endpoint[P <: HLinx.HList, R, W](method: Method,
                                              path: HLinx[P],
                                              read: EntityDecoder[F, R] = Read.No,
                                              write: EntityEncoder[F, W] = Write.No)

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
