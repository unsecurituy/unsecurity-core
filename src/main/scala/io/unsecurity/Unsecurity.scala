package io.unsecurity

import cats.Monad
import cats.effect.IO
import io.unsecurity.Unsecure.{QueryParams, UnsecureEndpoint}
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.QueryParam
import no.scalabin.http4s.directives.Directive
import org.http4s.{EntityEncoder, Method}

sealed trait MediaType[F[_], T] {
  def entityEncoder: EntityEncoder[F, T]
}
object application {
  case class json[F[_], T]()(implicit val entityEncoder: EntityEncoder[F, T]) extends MediaType[F, T]
}

class Unsecurity[F[_]: Monad, USER <: AuthenticatedUser[_, _]] {
  def safe: Safe[F, USER] = new Safe[F, USER]()

  def unsecuredRoute[PathParams <: HList](route: HLinx[PathParams]): UnsecureEndpoint[F, PathParams, HNil] = {

    new UnsecureEndpoint[F, PathParams, HNil](route, QueryParams.empty)
  }
}

object Unsecurity {
  def apply[F[_]: Monad, USER <: AuthenticatedUser[_, _]]: Unsecurity[F, USER] = new Unsecurity()
}

trait AuthenticatedUser[ID, PROFILE] {
  def id: ID

  def profile: PROFILE
}

object PathParams {
  def unapply[B <: HList](arg: B): Option[B] = None
}

class AlwaysAllow[USER <: AuthenticatedUser[_, _]] extends SecurityPredicate[USER](_ => true)

class AlwaysDeny[USER <: AuthenticatedUser[_, _]] extends SecurityPredicate[USER](_ => false)

case class CompleteRoute[F[_], USER <: AuthenticatedUser[_, _], IN, OUT, PathParams <: HList](
    route: HLinx[PathParams],
    method: Method,
    queryParams: HList,
    authorization: (IN, PathParams) => SecurityPredicate[USER],
    f: (USER, IN, PathParams) => Directive[F, OUT])

private case class RouteBuilderData[IN, PathParams <: HList, USER <: AuthenticatedUser[_, _]](
    route: HLinx[PathParams],
    queryParams: HList = HNil,
    authorization: (IN, PathParams) => SecurityPredicate[USER] = (_: IN, _: PathParams) => new AlwaysDeny[USER])

class Safe[F[_], USER <: AuthenticatedUser[_, _]] {
  def route[PathParams <: HList](path: HLinx[PathParams]): SafeRoute[F, USER, PathParams] =
    new SafeRoute[F, USER, PathParams](RouteBuilderData[Nothing, PathParams, USER](path))
}

class SafeRoute[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList](
    data: RouteBuilderData[_, PathParams, USER]) {
  def queryParams[A](param: QueryParam[A]): SafeRoute[F, USER, PathParams] = queryParams(param ::: HNil)

  def queryParams[A <: HList](params: A): SafeRoute[F, USER, PathParams] =
    new SafeRoute[F, USER, PathParams](data.copy(queryParams = params))

  def consumes[IN]: SafeRouteWithIn[F, USER, PathParams, IN] =
    new SafeRouteWithIn[F, USER, PathParams, IN](RouteBuilderData[IN, PathParams, USER](data.route, data.queryParams))

  def produces[OUT]: SafeRouteWithOut[F, USER, PathParams, OUT] = ???

  //new SafeRouteWithOut[F, USER, PathParams, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithIn[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN](
    data: RouteBuilderData[IN, PathParams, USER]) {
  def produces[OUT]: SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data)
}

class SafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](
    data: RouteBuilderData[Nothing, PathParams, USER]) {
  def consumes[IN]: SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???

  //new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
  def authorization(authRule: SecurityPredicate[USER]): CompletableSafeRouteWithOut[F, USER, PathParams, OUT] = ???

  // new CompletableSafeRouteWithOut[F, USER, PathParams, OUT](data.copy(authorization = authRule))
}

class SafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](
    data: RouteBuilderData[IN, PathParams, USER]) {
  def authorization[A <: AuthenticatedUser[_, _]](authRule: (IN, PathParams) => SecurityPredicate[A])
    : CompletableSafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???

  //new CompletableSafeRouteWithInAndOut[F, USER, IN, OUT, PathParams](data.copy(authorization = authRule))
}

class CompletableSafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](
    data: RouteBuilderData[Nothing, PathParams, USER]) {
  def GET(f: (USER, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, Nothing, OUT, PathParams] =
    new CompleteRoute[F, USER, Nothing, OUT, PathParams](
      route = data.route,
      method = Method.GET,
      queryParams = data.queryParams,
      authorization = data.authorization,
      f = (user, _, pp) => f(user, pp)
    )
}

class CompletableSafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](
    data: RouteBuilderData[IN, PathParams, USER]) {
  def POST(f: (USER, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, IN, OUT, PathParams] =
    new CompleteRoute[F, USER, IN, OUT, PathParams](route = data.route,
                                                    method = Method.POST,
                                                    queryParams = data.queryParams,
                                                    authorization = data.authorization,
                                                    f)
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
    override def id: String = "Kaare"

    override def profile: MyProfile = MyProfile("Kaare Nilsen", Set("admin"), Set("myFeature"))
  }

  case class HasRole(role: String)
      extends SecurityPredicate[MyAuthenticatedUser](userProfile => userProfile.profile.roles.contains(role))

  case class HasAccessToFeature(feature: String)
      extends SecurityPredicate[MyAuthenticatedUser](userProfile => userProfile.profile.roles.contains(feature))

  case class BodyContains(content: String, myRequest: MyRequest)
      extends SecurityPredicate[MyAuthenticatedUser](userProfile => myRequest.in.contains(content))

  case class MyResponse(result: String)

  case class MyRequest(in: String)

  val unsecurity = Unsecurity[IO, MyAuthenticatedUser]

  val aRoute =
    unsecurity.safe
      .route(Root / "aRequest" / param[Int]("intParam") / param[String]("stringParam"))
      .consumes[MyRequest] //application.json by magic somehow makes sure there is an decoder
      .produces[MyResponse] //application.json by magic somehow makes sure there is an encoder
      .authorization((myRequest, pathParams) =>
        HasRole("admin") || HasAccessToFeature("myFeature") && BodyContains("kaare", myRequest))
      .POST {
        case (user, myRequest, intParam ::: stringParam ::: HNil) =>
          Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }

  val otherRoute =
    unsecurity.safe
      .route(Root / "aRequest" / param[Int]("intParam") / param[String]("stringParam"))
      .queryParams(qParam[Int]("offset"))
      .consumes[MyRequest]
      .produces[MyResponse]
      .authorization((myRequest, pathParams) => HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST { (user, myRequest, pathParams) =>
        val (intParam, stringParam) = pathParams.tupled
        Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }

  val thirdRoute =
    unsecurity.safe
      .route(Root / "aRequest")
      .queryParams(qParam[Int]("offset") & qParam[String]("bar"))
      .consumes[MyRequest]
      .produces[MyResponse]
      .authorization((myRequest, pathParams) => HasRole("admin") || HasAccessToFeature("myFeature"))
      .POST { (user, myRequest, _) =>
        Directive.success(MyResponse(s"Hello ${user.profile.name}, you requested ${myRequest.in}"))
      }
}
