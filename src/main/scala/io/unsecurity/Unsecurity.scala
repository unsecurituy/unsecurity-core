package io.unsecurity

import cats.effect.IO
import no.scalabin.http4s.directives.Directive

object Unsecurity {
  def safe[F[_]]: Safe[F]     = ???
  def unsafe[F[_]]: Unsafe[F] = ???
}

class Route[F[_]](
    path: String,
    queryParams: List[String],
    accepts: List[String],
    produces: List[String],
    authorization: Option[String],
    method: String,
    f: () => String
)

class Unsafe[F[_]] {
  def route(path: String) = ???
}

class Safe[F[_]] {
  def route(path: String): SafeRoute[F] = ???
}

class SafeRoute[F[_]](
    path: String,
    queryParams: List[String],
    accepts: List[String],
    produces: List[String],
    authorization: Option[String]
) extends Safe {
  def queryParams(param: String): SafeRoute[F]                       = ???
  def accepts(mediaType: String*): SafeRoute[F]                      = ???
  def produces(mediaType: String*): SafeRoute[F]                     = ???
  def authorization[A](authRule: SecurityPredicate[A]): SafeRoute[F] = ???
  def GET[A](f: () => Directive[F, A]): Route[F]                     = ???
}

class SecurityPredicate[-A](predicate: A => Boolean) {
  def ||[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) || other(a))

  def &&[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) && other(a))

  def unary_!(): SecurityPredicate[A] = new SecurityPredicate[A](a => !predicate(a))

  def apply(a: A) = predicate(a)
}

object Test {
  val aRoute: Route[IO] = Unsecurity
    .safe[IO]
    .route("")
    .GET[String] { () =>
      Directive.success("Hello")
    }
}

/*
unsecurity
    .safe|unsafe
    .route("root" / 'id[String])
    .queryParams?(param[List[String]("names)])
    .consumes?(if requesttype with body)[MyRequest](application/json, appliation/xml)
    .produces[MyResponse](application/json)
    .authorization?((HasRole("admin) && HasAcessToParam("knut")))
    .POST{ (body: BalleRequest, knut: Int, names: List[String]) =>
      //body
    }
 */
