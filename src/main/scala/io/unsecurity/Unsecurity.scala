package io.unsecurity

import cats.effect.IO
import io.unsecurity.hlinx.HLinx.{HList, LinkFragment, _}
import no.scalabin.http4s.directives.Directive

class Unsecurity[USER <: AuthenticatedUser[_, _]] {
  def safe[F[_]]: Safe[F, USER] = ???
  //def unsafe[F[_]]: Unsafe[F] = ???
}

object Unsecurity {

  def apply[USER <: AuthenticatedUser[_, _]]: Unsecurity[USER] = new Unsecurity()
}

/**
  * Represents the Authenticated user
  * @tparam A The identity type
  */
trait AuthenticatedUser[A, B] {
  def id: A
  def profile: B
}

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

class CompleteRoute[F[_], USER <: AuthenticatedUser[_, _], ResponseType, PathParams <: HList](
//    path: LinkFragment[PathParams],
//    queryParams: Map[String, List[String]], //Eirik fikser et alternativ etterhvert
//    accepts: List[MediaType],
//    produces: List[MediaType],
//    authorization: Option[String],
//    method: String,
//    f: () => Directive[F, ResponseType]
)

class Safe[F[_], USER <: AuthenticatedUser[_, _]] {
  def route[PathParams <: HList](path: LinkFragment[PathParams]): SafeRoute[F, USER, PathParams] = new SafeRoute[F, USER, PathParams]
}

class SafeRoute[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList]() {
  def queryParams(param: String): SafeRoute[F, USER, PathParams]                       = ???
  def consumes[IN](mediaType: MediaType*): SafeRouteWithIn[F, USER, PathParams, IN]    = ???
  def produces[OUT](mediaType: MediaType*): SafeRouteWithOut[F, USER, PathParams, OUT] = ???
}

class SafeRouteWithIn[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN] {
  def produces[OUT](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???
}
class SafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT] {
  def consumes[IN](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT]                         = ???
  def authorization[A <: PredicateContext](authRule: SecurityPredicate[A]): SafeRouteWithOut[F, USER, PathParams, OUT] = ???
  def GET(f: (USER, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, OUT, PathParams]                         = ???
}

class SafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT] {
  def authorization[A <: PredicateContext](authRule: SecurityPredicate[A]): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???
  def POST(f: (USER, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, OUT, PathParams]                             = ???
}

class SecurityPredicate[A <: PredicateContext](predicate: A => Boolean) {
  def ||[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) || other(a))

  def &&[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) && other(a))

  def unary_!(): SecurityPredicate[A] = new SecurityPredicate[A](a => !predicate(a))

  def apply(a: A) = predicate(a)
}

object Test {
  case class MyProfile(name: String, roles: Set[String], features: Set[String])

  class MyAuthenticatedUser extends AuthenticatedUser[String, MyProfile] {
    override def id: String         = "Kaare"
    override def profile: MyProfile = MyProfile("Kaare Nilsen", Set("admin"), Set("myFeature"))
  }

  case class UserProfilePredicateContext(profile: MyProfile) extends PredicateContext
  case class HasRole(role: String)                           extends SecurityPredicate[UserProfilePredicateContext](userProfile => userProfile.profile.roles.contains(role))
  case class HasAccessToFeature(feature: String)
      extends SecurityPredicate[UserProfilePredicateContext](userProfile => userProfile.profile.roles.contains(feature))

  case class MyResponse(result: String)
  case class MyRequest(in: String)

  val unsecurity = Unsecurity[MyAuthenticatedUser]

  val aRoute =
    unsecurity
      .safe[IO]
      .route(Root / "aRequest" / Param[Int]("intParam") / Param[String]("stringParam"))
      .consumes[MyRequest](application.json) //application.json by magic somehow makes sure there is an decoder
      .produces[MyResponse](application.json) //application.json by magic somehow makes sure there is an encoder
      .authorization(HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST {
        case (user, myRequest, intParam :: stringParam :: HNil) =>
          Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }

  val otherRoute =
    unsecurity
      .safe[IO]
      .route(Root / "aRequest" / Param[Int]("intParam") / Param[String]("stringParam"))
      .consumes[MyRequest](application.json)
      .produces[MyResponse](application.json)
      .authorization(HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST { (user, myRequest, pathParams) =>
        val (intParam, stringParam) = pathParams.tupled
        Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }

  val thirdRoute =
    unsecurity
      .safe[IO]
      .route(Root / "aRequest")
      .consumes[MyRequest](application.json)
      .produces[MyResponse](application.json)
      .authorization(HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST { (user, myRequest, _) =>
        Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }
}
