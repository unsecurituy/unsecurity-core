package io.unsecurity

import cats.Always
import cats.effect.IO
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.QueryParam
import no.scalabin.http4s.directives.Directive
import org.http4s.Method

class Unsecurity[F[_], USER <: AuthenticatedUser[_, _]] {
  def safe: Safe[F, USER] = new Safe[F, USER]()
  //def unsafe[F[_]]: Unsafe[F] = ???
}

object Unsecurity {

  def apply[F[_], USER <: AuthenticatedUser[_, _]]: Unsecurity[F, USER] = new Unsecurity()
}

/**
  * Represents the Authenticated user
  *
  * @tparam A The identity type
  */
trait AuthenticatedUser[ID, PROFILE] {
  def id: ID
  def profile: PROFILE
}

trait MediaType
object application {
  object json extends MediaType
}

object PathParams {
  def unapply[B <: HList](arg: B): Option[B] = None
}

private class AlwaysAllow extends SecurityPredicate[AuthenticatedUser[_, _]](_ => true)
private class AlwaysDeny  extends SecurityPredicate[AuthenticatedUser[_, _]](_ => false)

class CompleteRoute[F[_], USER <: AuthenticatedUser[_, _], ResponseType, PathParams <: HList](
    route: HLinx[PathParams],
    method: Method,
    queryParams: HList,
    consumes: List[MediaType],
    produces: List[MediaType],
    authorization: SecurityPredicate[USER]
)

case class CompleteRouteBuilderData[PathParams <: HList, USER <: AuthenticatedUser[_, _]](
    route: HLinx[PathParams],
    queryParams: HList = HNil,
    consumes: List[MediaType] = Nil,
    produces: List[MediaType] = Nil,
    authorization: SecurityPredicate[USER] = new SecurityPredicate[USER](_ => false)
)

class Safe[F[_], USER <: AuthenticatedUser[_, _]] {
  def route[PathParams <: HList](path: HLinx[PathParams]): SafeRoute[F, USER, PathParams] =
    new SafeRoute[F, USER, PathParams](CompleteRouteBuilderData[PathParams, USER](path))
}

class SafeRoute[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList](data: CompleteRouteBuilderData[PathParams, USER]) {
  def queryParams[A](param: QueryParam[A]): SafeRoute[F, USER, PathParams] = queryParams(param ::: HNil)
  def queryParams[A <: HList](params: A): SafeRoute[F, USER, PathParams]   = new SafeRoute[F, USER, PathParams](data.copy(queryParams = params))
  def consumes[IN](mediaType: MediaType*): SafeRouteWithIn[F, USER, PathParams, IN] =
    new SafeRouteWithIn[F, USER, PathParams, IN](data.copy(consumes = mediaType.toList))
  def produces[OUT](mediaType: MediaType*): SafeRouteWithOut[F, USER, PathParams, OUT] =
    new SafeRouteWithOut[F, USER, PathParams, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithIn[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN](data: CompleteRouteBuilderData[PathParams, USER]) {
  def produces[OUT](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: CompleteRouteBuilderData[PathParams, USER]) {
  def consumes[IN](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
  def authorization(authRule: SecurityPredicate[USER]): CompletableSafeRouteWithOut[F, USER, PathParams, OUT] =
    new CompletableSafeRouteWithOut[F, USER, PathParams, OUT](data.copy(authorization = authRule))
}

class SafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: CompleteRouteBuilderData[PathParams, USER]) {
  def authorization[A <: AuthenticatedUser[_, _]](
      authRule: (IN, PathParams) => SecurityPredicate[A]): CompletableSafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???
}

class CompletableSafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: CompleteRouteBuilderData[PathParams, USER]) {
  def GET(f: (USER, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, OUT, PathParams] =
    new CompleteRoute[F, USER, OUT, PathParams](data.route, Method.GET, data.queryParams, data.consumes, data.produces, data.authorization)
}

class CompletableSafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: CompleteRouteBuilderData[PathParams, USER]) {
  def POST(f: (USER, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, OUT, PathParams] = ???
}

class SecurityPredicate[USER <: AuthenticatedUser[_, _]](predicate: USER => Boolean) {
  def ||[B <: USER](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) || other(a))

  def &&[B <: USER](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) && other(a))

  def unary_!(): SecurityPredicate[USER] = new SecurityPredicate[USER](a => !predicate(a))

  def apply(a: USER) = predicate(a)
}

object Test {
  case class MyProfile(name: String, roles: Set[String], features: Set[String])

  class MyAuthenticatedUser extends AuthenticatedUser[String, MyProfile] {
    override def id: String         = "Kaare"
    override def profile: MyProfile = MyProfile("Kaare Nilsen", Set("admin"), Set("myFeature"))
  }

  case class HasRole(role: String)                               extends SecurityPredicate[MyAuthenticatedUser](userProfile => userProfile.profile.roles.contains(role))
  case class HasAccessToFeature(feature: String)                 extends SecurityPredicate[MyAuthenticatedUser](userProfile => userProfile.profile.roles.contains(feature))
  case class BodyContains(content: String, myRequest: MyRequest) extends SecurityPredicate[MyAuthenticatedUser](userProfile => myRequest.in.contains(content))

  case class MyResponse(result: String)
  case class MyRequest(in: String)

  val unsecurity = Unsecurity[IO, MyAuthenticatedUser]

  val aRoute =
    unsecurity.safe
      .route(Root / "aRequest" / param[Int]("intParam") / param[String]("stringParam"))
      .consumes[MyRequest](application.json) //application.json by magic somehow makes sure there is an decoder
      .produces[MyResponse](application.json) //application.json by magic somehow makes sure there is an encoder
      .authorization((myRequest, pathParams) => HasRole("admin") || HasAccessToFeature("myFeature") && BodyContains("kaare", myRequest))
      .POST {
        case (user, myRequest, intParam ::: stringParam ::: HNil) =>
          Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }

  val otherRoute =
    unsecurity.safe
      .route(Root / "aRequest" / param[Int]("intParam") / param[String]("stringParam"))
      .queryParams(qParam[Int]("offset"))
      .consumes[MyRequest](application.json)
      .produces[MyResponse](application.json)
      .authorization((myRequest, pathParams) => HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST { (user, myRequest, pathParams) =>
        val (intParam, stringParam) = pathParams.tupled
        Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }

  val thirdRoute =
    unsecurity.safe
      .route(Root / "aRequest")
      .queryParams(qParam[Int]("offset") & qParam[String]("bar"))
      .consumes[MyRequest](application.json)
      .produces[MyResponse](application.json)
      .authorization((myRequest, pathParams) => HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST { (user, myRequest, _) =>
        Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }
}
