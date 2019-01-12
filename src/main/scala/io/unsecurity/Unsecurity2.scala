package io.unsecurity

import cats.{Applicative, Monad}
import cats.effect.IO
import io.circe.{Decoder, Encoder}
import io.unsecurity.Unsecure.PathMatcher
import io.unsecurity.hlinx.HLinx
import io.unsecurity.hlinx.HLinx._
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.{DecodeResult, EntityDecoder, EntityEncoder, MediaRange, Message, Method}

object Unsecurity2 {

  object Read {
    case object No extends Read[Unit]
    def json[A: Decoder]: Read[A] = ???
  }
  sealed trait Read[+A]

  object Write {
    case object No extends Write[Unit] {
      override def entityEncoder[F[_]]: _root_.org.http4s.EntityEncoder[F, Unit] = ???
    }
    def json[A: Encoder]: Write[A] = new Write[A] {
      override def entityEncoder[F[_]]: EntityEncoder[F, A] =
        new EntityEncoder[] {}
        ???
    }
  }
  sealed trait Write[A] {
    def entityEncoder[F[_]]: EntityEncoder[F, A]
  }

  trait Safe[F[_], C, W] extends Completable[F, C, W] {
    def authorization(predicate: C => Boolean): Completable[F, C, W]
    override def resolve[C2](f: C => F[C2]): Safe[F, C2, W]
  }

  trait Authenticator[F[_], A] {
    def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Safe[F, (P, R, A), W]
    def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[F, (P, R), W] =
      ???
  }

  class UnsecureAuthenticator[F[_]: Monad] extends Authenticator[F, Unit] {
    override def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Safe[F, (P, R, Unit), W] = ???
    override def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[F, (P, R), W] = {
      MyCompletable[F, (P, R), W](
        pathMatcher = Unsecure.createPathMatcher[F, P](endpoint.path).asInstanceOf[PathMatcher[F, Any]],
        methodMap = Map(
          endpoint.method -> { pp: P =>
            for {
              req <- Directive.request
            } yield {
              (pp, ???): (P, R)
            }
          }.asInstanceOf[Any => Directive[F, (P, R)]]
        ),
        entityEncoder = ???
      )
    }

  }

  trait Completable[F[_], C, W] {
    def run(f: C => Directive[F, W]): Complete[F]
    def resolve[C2](f: C => F[C2]): Completable[F, C2, W]
  }

  case class MyCompletable[F[_], C, W](
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Completable[F, C, W] {
    override def run(f: C => Directive[F, W]): Complete[F]         = ???
    override def resolve[C2](f: C => F[C2]): Completable[F, C2, W] = ???
  }

  trait Complete[F[_]] {
    def ||(next: Complete[F]): Complete[F]
  }

  case class MyComplete[F[_]](pathMatcher: PartialFunction[String, Any],
                              methodMap: Map[Method, Any => ResponseDirective[F]])
      extends Complete[F] {
    override def ||(next: _root_.io.unsecurity.Unsecurity2.Complete[F]): _root_.io.unsecurity.Unsecurity2.Complete[F] =
      ???
  }

  case class Endpoint[P <: HLinx.HList, R, W](method: Method,
                                              path: HLinx[P],
                                              read: Read[R] = Read.No,
                                              write: Write[W] = Write.No)

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

}
