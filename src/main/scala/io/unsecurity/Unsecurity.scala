package io.unsecurity

import cats.effect.IO
import io.unsecurity.hlinx.HLinx.{HList, LinkFragment, _}
import no.scalabin.http4s.directives.Directive

class Unsecurity[USERPROFILE <: UserProfile] {
  def safe[F[_]]: Safe[F, USERPROFILE] = ???
  //def unsafe[F[_]]: Unsafe[F] = ???
}

object Unsecurity {
  def apply[USERPROFILE <: UserProfile]: Unsecurity[USERPROFILE] = new Unsecurity()
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

class CompleteRoute[F[_], USERPROFILE <: UserProfile, ResponseType, PathParams <: HList](
//    path: LinkFragment[PathParams],
//    queryParams: Map[String, List[String]], //Eirik fikser et alternativ etterhvert
//    accepts: List[MediaType],
//    produces: List[MediaType],
//    authorization: Option[String],
//    method: String,
//    f: () => Directive[F, ResponseType]
)

class Safe[F[_], USERPROFILE <: UserProfile] {
  def route[PathParams <: HList](path: LinkFragment[PathParams]): SafeRoute[F, USERPROFILE, PathParams] = new SafeRoute[F, USERPROFILE, PathParams]
}

class SafeRoute[F[_], USERPROFILE <: UserProfile, PathParams <: HList]() {
  def queryParams(param: String): SafeRoute[F, USERPROFILE, PathParams]                       = ???
  def consumes[IN](mediaType: MediaType*): SafeRouteWithIn[F, USERPROFILE, PathParams, IN]    = ???
  def produces[OUT](mediaType: MediaType*): SafeRouteWithOut[F, USERPROFILE, PathParams, OUT] = ???
}

class SafeRouteWithIn[F[_], USERPROFILE <: UserProfile, PathParams <: HList, IN] {
  def produces[OUT](mediaType: MediaType*): SafeRouteWithInAndOut[F, USERPROFILE, PathParams, IN, OUT] = ???
}
class SafeRouteWithOut[F[_], USERPROFILE <: UserProfile, PathParams <: HList, OUT] {
  def consumes[IN](mediaType: MediaType*): SafeRouteWithInAndOut[F, USERPROFILE, PathParams, IN, OUT]                         = ???
  def authorization[A <: PredicateContext](authRule: SecurityPredicate[A]): SafeRouteWithOut[F, USERPROFILE, PathParams, OUT] = ???
  def GET(f: (USERPROFILE, PathParams) => Directive[F, OUT]): CompleteRoute[F, USERPROFILE, OUT, PathParams]                  = ???
}

class SafeRouteWithInAndOut[F[_], USERPROFILE <: UserProfile, PathParams <: HList, IN, OUT] {
  def authorization[A <: PredicateContext](authRule: SecurityPredicate[A]): SafeRouteWithInAndOut[F, USERPROFILE, PathParams, IN, OUT] = ???
  def POST(f: (USERPROFILE, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, USERPROFILE, OUT, PathParams]                      = ???
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
  val aRoute: CompleteRoute[IO, MyProfile, MyResponse, ::[String, ::[Int, HNil]]] =
    Unsecurity[MyProfile]
      .safe[IO]
      .route(Root / "aRequest" / Param[Int]("intParam") / Param[String]("stringParam"))
      .consumes[MyRequest](application.json) //application.json by magic somehow makes sure there is an encoder
      .produces[MyResponse](application.json) //application.json by magic somehow makes sure there is an encoder
      .POST {
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
