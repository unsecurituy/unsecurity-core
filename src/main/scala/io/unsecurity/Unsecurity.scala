package io.unsecurity

import cats.effect.IO
import io.unsecurity.hlinx.HLinx.{HList, LinkFragment, _}
import no.scalabin.http4s.directives.Directive

object Unsecurity {
  def safe[F[_]]: Safe[F] = ???
  //def unsafe[F[_]]: Unsafe[F] = ???
}

/**
  * Represents the Authenticated user
  * @tparam A The identity type
  */
trait AuthenticatedUser[A] {
  def id: A
}

trait UserProfile

trait AuthorizedUser

trait MediaType
object application {
  object json extends MediaType
}

trait PredicateContext

object PathParams {
  def unapply[B <: HList](arg: B): Option[B] = None
}

private class EmptyPredicateContext extends PredicateContext
private object AlwaysAllow          extends SecurityPredicate[EmptyPredicateContext](_ => true)
private object AlwaysDeny           extends SecurityPredicate[EmptyPredicateContext](_ => false)

class CompleteRoute[F[_], ResponseType, PathParams <: HList](
    path: LinkFragment[PathParams],
    queryParams: Map[String, List[String]], //Eirik fikser et alternativ etterhvert
    accepts: List[MediaType],
    produces: List[MediaType],
    authorization: Option[String],
    method: String,
    f: () => Directive[F, ResponseType]
)

class Safe[F[_]] {
  def route[PathParams <: HList](path: LinkFragment[PathParams]): SafeRoute[F, PathParams] = new SafeRoute[F, PathParams](
    path,
    Map.empty,
    List.empty,
    List.empty,
    AlwaysDeny
  )
}

class SafeRoute[F[_], PathParams <: HList](
    path: LinkFragment[PathParams],
    queryParams: Map[String, List[String]],
    accepts: List[MediaType],
    produces: List[MediaType],
    authorization: SecurityPredicate[_]
) extends Safe {
  def queryParams(param: String): SafeRoute[F, PathParams]                                                                      = ???
  def consumes[IN](mediaType: MediaType*): SafeRoutWithMethod[F, PathParams, IN, Nothing]                                       = ???
  def produces[OUT](mediaType: MediaType*): SafeRoutWithMethod[F, PathParams, Nothing, OUT]                                     = ???
  def authorization[A <: PredicateContext](authRule: SecurityPredicate[A]): SafeRoutWithMethod[F, PathParams, Nothing, Nothing] = ???

}

class SafeRoutWithMethod[F[_], PathParams <: HList, IN, OUT](
    path: LinkFragment[PathParams],
    queryParams: Map[String, List[String]],
    accepts: List[MediaType],
    produces: List[MediaType],
    authorization: SecurityPredicate[_]
) extends SafeRoute[F, PathParams](path, queryParams, accepts, produces, authorization) {
  def GET[USERPROFILE <: UserProfile](f: (USERPROFILE, PathParams) => Directive[F, OUT]): CompleteRoute[F, OUT, PathParams]  = ???
  def POST[USERPROFILE <: UserProfile](f: (USERPROFILE, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, OUT, PathParams] = ???
}

class SecurityPredicate[A <: PredicateContext](predicate: A => Boolean) {
  def ||[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) || other(a))

  def &&[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) && other(a))

  def unary_!(): SecurityPredicate[A] = new SecurityPredicate[A](a => !predicate(a))

  def apply(a: A) = predicate(a)
}

object Test {
  class MyAuthenticatedUser extends AuthenticatedUser[String] {
    override def id: String = "Kaare"
  }

  case class MyProfile(name: String) extends UserProfile

  case class MyResponse(result: String)
  case class MyRequest(in: String)

  val :: = HCons
  val aRoute: CompleteRoute[IO, MyResponse, ::[String, ::[Int, HNil]]] =
    Unsecurity
      .safe[IO]
      .route(Root / "balle" / Param[Int]("knut") / Param[String]("ola"))
      .consumes[MyRequest](application.json) //application.json by magic somehow makes sure there is an encoder
      .produces[MyResponse](application.json) //application.json by magic somehow makes sure there is an encoder
      .POST[MyProfile] {
        case (user, myRequest, knut :: ola :: HNil) =>
          Directive.success(MyResponse(s"Hello ${user.name}, you requested ${myRequest.in}"))
      }
}

/*
unsecurity
    .safe|unsafe
    .route("root" / 'id[String])
    .queryParams?(qp[Int]("int") & qp[Option[String]]("str"))
    .consumes[MyRequest](application/json, appliation/xml)
    .produces[MyResponse](application/json)
    .authorization?((HasRole("admin) && HasAcessToParam("knut")))
    .POST{ (user: MyProfile, body: MyRequest, (knut: Int), (int: Int, str: Option[String])) =>
      myResponse
    }
 */
