package io.unsecurity

import cats.effect.IO
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.QueryParam
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.{Method, Response}

class Unsecurity[F[_], USER <: AuthenticatedUser[_, _]] {
  def safe: Safe[F, USER] = new Safe[F, USER]()
  def unsafe: Unsafe[F]   = new Unsafe[F]()
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

class AlwaysAllow[USER <: AuthenticatedUser[_, _]] extends SecurityPredicate[USER](_ => true)
class AlwaysDeny[USER <: AuthenticatedUser[_, _]]  extends SecurityPredicate[USER](_ => false)

class CompleteRoute2[F[_], USER <: AuthenticatedUser[_, _], IN, ResponseType, PathParams <: HList](
    route: List[SimpleLinx],
    method: Method,
    pathDirective: Directive[F, Response[F]],
    resultDirective: PathParams => Directive[F, Response[F]]
)

case class CompleteRoute[F[_], USER <: AuthenticatedUser[_, _], IN, OUT, PathParams <: HList](
    route: HLinx[PathParams],
    method: Method,
    queryParams: HList,
    consumes: List[MediaType],
    produces: List[MediaType],
    authorization: (IN, PathParams) => SecurityPredicate[USER],
    f: (USER, IN, PathParams) => Directive[F, OUT]
)

private case class RouteBuilderData[IN, PathParams <: HList, USER <: AuthenticatedUser[_, _]](
    route: HLinx[PathParams],
    queryParams: HList = HNil,
    consumes: List[MediaType] = Nil,
    produces: List[MediaType] = Nil,
    authorization: (IN, PathParams) => SecurityPredicate[USER] = (_: IN, _: PathParams) => new AlwaysDeny[USER]
)

class Unsafe[F[_]] {
  def route[PathParams <: HList](path: HLinx[PathParams]): UnsafeRoute[F, PathParams] =
    new UnsafeRoute[F, PathParams](RouteBuilderData[Nothing, PathParams, Nothing](path))
}

class UnsafeRoute[F[_], PathParams <: HList](data: RouteBuilderData[_, PathParams, Nothing]) {
  def produces[OUT]: CompletableUnsafeRouteWithOut[F, PathParams, OUT] =
    new CompletableUnsafeRouteWithOut[F, PathParams, OUT](
      data = new RouteBuilderData[Nothing, PathParams, Nothing](
        route = data.route,
        queryParams = data.queryParams,
        consumes = List.empty,
        produces = List(application.json),
        authorization = (_, _) => new AlwaysAllow[Nothing]
      )
    )
}

case class UnsafeGetRoute[F[_], PathParams <: HList](
    groupByPath: List[SimpleLinx],
    pathDirective: Directive[F, PathParams],
    method: Method,
    f: PathParams => ResponseDirective[IO]
)

class CompletableUnsafeRouteWithOut[F[_], PathParams <: HList, OUT](data: RouteBuilderData[Nothing, PathParams, Nothing]) {
  def GET(f: PathParams => Directive[F, OUT]): CompleteRoute[F, Nothing, Nothing, OUT, PathParams] = {
//    new CompleteRoute[F, Nothing, Nothing, OUT, PathParams](
//      route = data.route,
//      method = Method.GET,
//      queryParams = data.queryParams,
//      consumes = data.consumes,
//      produces = data.produces,
//      authorization = data.authorization,
//      f = (_, _, pp) => f(pp)
//    )

    UnsafeGetRoute(
      groupByPath = data.route.toSimple.reverse,
      ???,
      ???,
      ???
    )

    // path abstraksjon som kan grupperes på
    // path-direktiv : PATHPARAMS
    // method for å kunne lage et direktiv
    // f : PATHPARAMS => ResponseDirective[IO]

    ???
  }
}

class Safe[F[_], USER <: AuthenticatedUser[_, _]] {
  def route[PathParams <: HList](path: HLinx[PathParams]): SafeRoute[F, USER, PathParams] =
    new SafeRoute[F, USER, PathParams](RouteBuilderData[Nothing, PathParams, USER](path))
}

class SafeRoute[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList](data: RouteBuilderData[_, PathParams, USER]) {
  def queryParams[A](param: QueryParam[A]): SafeRoute[F, USER, PathParams] = queryParams(param ::: HNil)
  def queryParams[A <: HList](params: A): SafeRoute[F, USER, PathParams]   = new SafeRoute[F, USER, PathParams](data.copy(queryParams = params))
  def consumes[IN](mediaType: MediaType*): SafeRouteWithIn[F, USER, PathParams, IN] =
    new SafeRouteWithIn[F, USER, PathParams, IN](RouteBuilderData[IN, PathParams, USER](data.route, data.queryParams, mediaType.toList, data.produces))
  def produces[OUT](mediaType: MediaType*): SafeRouteWithOut[F, USER, PathParams, OUT] = ???
  //new SafeRouteWithOut[F, USER, PathParams, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithIn[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN](data: RouteBuilderData[IN, PathParams, USER]) {
  def produces[OUT](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: RouteBuilderData[Nothing, PathParams, USER]) {
  def consumes[IN](mediaType: MediaType*): SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???
  //new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data.copy(produces = mediaType.toList))
  def authorization(authRule: SecurityPredicate[USER]): CompletableSafeRouteWithOut[F, USER, PathParams, OUT] = ???
  // new CompletableSafeRouteWithOut[F, USER, PathParams, OUT](data.copy(authorization = authRule))
}

class SafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: RouteBuilderData[IN, PathParams, USER]) {
  def authorization[A <: AuthenticatedUser[_, _]](
      authRule: (IN, PathParams) => SecurityPredicate[A]): CompletableSafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???
  //new CompletableSafeRouteWithInAndOut[F, USER, IN, OUT, PathParams](data.copy(authorization = authRule))
}

class CompletableSafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: RouteBuilderData[Nothing, PathParams, USER]) {
  def GET(f: (USER, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, Nothing, OUT, PathParams] =
    new CompleteRoute[F, USER, Nothing, OUT, PathParams](
      route = data.route,
      method = Method.GET,
      queryParams = data.queryParams,
      consumes = data.consumes,
      produces = data.produces,
      authorization = data.authorization,
      f = (user, _, pp) => f(user, pp)
    )
}

class CompletableSafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: RouteBuilderData[IN, PathParams, USER]) {
  def POST(f: (USER, IN, PathParams) => Directive[F, OUT]): CompleteRoute[F, USER, IN, OUT, PathParams] =
    new CompleteRoute[F, USER, IN, OUT, PathParams](route = data.route,
                                                    method = Method.POST,
                                                    queryParams = data.queryParams,
                                                    consumes = data.consumes,
                                                    produces = data.produces,
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
