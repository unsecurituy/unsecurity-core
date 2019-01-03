package io.unsecurity

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

private class AlwaysAllow[USER <: AuthenticatedUser[_, _]] extends SecurityPredicate[USER](_ => Some(HNil))
private class AlwaysDeny[USER <: AuthenticatedUser[_, _]]  extends SecurityPredicate[USER](_ => None)

class CompleteRoute[F[_], USER <: AuthenticatedUser[_, _], ResponseType, PathParams <: HList](
    route: HLinx[PathParams],
    method: Method,
    queryParams: HList,
    consumes: List[MediaType],
    produces: List[MediaType],
    authorization: SecurityPredicate[USER]
)

private case class RouteBuilderData[PathParams <: HList, USER <: AuthenticatedUser[_, _]](
    route: HLinx[PathParams],
    queryParams: HList = HNil,
    consumes: List[MediaType] = Nil,
    produces: List[MediaType] = Nil,
    authorization: SecurityPredicate[USER] = new AlwaysDeny[USER]
)

class Safe[F[_], USER <: AuthenticatedUser[_, _]] {
  def route[PathParams <: HList](path: HLinx[PathParams]): SafeRoute[F, USER, PathParams] =
    new SafeRoute[F, USER, PathParams](RouteBuilderData[PathParams, USER](path))
}

class SafeRoute[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList](data: RouteBuilderData[PathParams, USER]) {
  def queryParams[A](param: QueryParam[A]): SafeRoute[F, USER, PathParams] = queryParams(param ::: HNil)
  def queryParams[A <: HList](params: A): SafeRoute[F, USER, PathParams]   = new SafeRoute[F, USER, PathParams](data.copy(queryParams = params))
  def consumes[IN](mediaType: MediaType*): SafeRouteWithIn[F, USER, PathParams, IN] =
    new SafeRouteWithIn[F, USER, PathParams, IN](data.copy(consumes = mediaType.toList))
  def produces[OUT](mediaType: MediaType*): SafeRouteWithOut[F, USER, PathParams, OUT] =
    new SafeRouteWithOut[F, USER, PathParams, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithIn[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN](data: RouteBuilderData[PathParams, USER]) {
  def produces[OUT](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: RouteBuilderData[PathParams, USER]) {
  def consumes[IN](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
  def authorization(authRule: SecurityPredicate[USER]): CompletableSafeRouteWithOut[F, USER, PathParams, OUT] =
    new CompletableSafeRouteWithOut[F, USER, PathParams, OUT](data.copy(authorization = authRule))
}

class SafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: RouteBuilderData[PathParams, USER]) {
  def authorization[A <: AuthenticatedUser[_, _]](
      authRule: (IN, PathParams) => SecurityPredicate[A]): CompletableSafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???
    //new CompletableSafeRouteWithInAndOut[F, USER, IN, OUT, PathParams](data.copy(authorization = authRule))
}

class CompletableSafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: RouteBuilderData[PathParams, USER]) {
  def GET(f: (USER, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, OUT, PathParams] =
    new CompleteRoute[F, USER, OUT, PathParams](data.route, Method.GET, data.queryParams, data.consumes, data.produces, data.authorization)
}

class CompletableSafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: RouteBuilderData[PathParams, USER]) {
  def POST(f: (USER, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, OUT, PathParams] =
    new CompleteRoute[F, USER, OUT, PathParams](data.route, Method.POST, data.queryParams, data.consumes, data.produces, data.authorization)
}

class SecurityPredicate[USER <: AuthenticatedUser[_, _]](predicate: USER => Option[HList]) {
  def ||[B <: USER](other: SecurityPredicate[B]) =
    new SecurityPredicate[B](a =>
      (predicate(a), other(a)) match {
        case (Some(aCtx), Some(bCtx)) => Some(aCtx) //TODO: Append bCtx
        case (Some(aCtx), None)       => Some(aCtx)
        case (None, Some(bCtx))       => Some(bCtx)
        case (None, None)             => None
    })

  def &&[B <: USER](other: SecurityPredicate[B]) =
    new SecurityPredicate[B](a =>
      (predicate(a), other(a)) match {
        case (Some(aCtx), Some(bCtx)) => Some(aCtx) //TODO: Append bCtx
        case _                        => None
    })

  def apply(a: USER): Option[HList] = predicate(a)
}

object Test {
  case class MyProfile(name: String, roles: Set[String], features: Set[String])

  class MyAuthenticatedUser extends AuthenticatedUser[String, MyProfile] {
    override def id: String         = "Kaare"
    override def profile: MyProfile = MyProfile("Kaare Nilsen", Set("admin"), Set("myFeature"))
  }

  case class HasRole(role: String)                               extends SecurityPredicate[MyAuthenticatedUser](userProfile => if (userProfile.profile.roles.contains(role)) Some(HNil) else None)
  case class HasAccessToFeature(feature: String)                 extends SecurityPredicate[MyAuthenticatedUser](userProfile => if (userProfile.profile.roles.contains(feature)) Some(HNil) else None)
  case class BodyContains(content: String, myRequest: MyRequest) extends SecurityPredicate[MyAuthenticatedUser](userProfile => if (myRequest.in.contains(content)) Some(HNil) else None)

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
