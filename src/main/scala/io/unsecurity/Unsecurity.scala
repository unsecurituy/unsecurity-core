package io.unsecurity

import cats.Monad
import cats.effect.IO
import io.unsecurity.hlinx.HLinx._
import io.unsecurity.hlinx.QueryParam
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, RequestDirectives, Result}
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, Method, Response, Status}

class Unsecurity[F[_]: Monad, USER <: AuthenticatedUser[_, _]] {
  def safe: Safe[F, USER] = new Safe[F, USER]()

  def unsafe[PathParams <: HList](route: HLinx[PathParams]): UnsafeRoute[F, PathParams] =
    new UnsafeRoute[F, PathParams](RouteBuilderData[Nothing, PathParams, Nothing](route))
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

case class CompleteRoute[F[_], USER <: AuthenticatedUser[_, _], IN, OUT, PathParams <: HList](route: HLinx[PathParams],
                                                                                              method: Method,
                                                                                              queryParams: HList,
                                                                                              authorization: (IN, PathParams) => SecurityPredicate[USER],
                                                                                              f: (USER, IN, PathParams) => Directive[F, OUT])

private case class RouteBuilderData[IN, PathParams <: HList, USER <: AuthenticatedUser[_, _]](route: HLinx[PathParams],
                                                                                              queryParams: HList = HNil,
                                                                                              authorization: (IN, PathParams) => SecurityPredicate[USER] =
                                                                                                (_: IN, _: PathParams) => new AlwaysDeny[USER])

class Unsafe[F[_]: Monad] {
  def route[PathParams <: HList](path: HLinx[PathParams]): UnsafeRoute[F, PathParams] =
    new UnsafeRoute[F, PathParams](RouteBuilderData[Nothing, PathParams, Nothing](path))
}

class UnsafeRoute[F[_]: Monad, PathParams <: HList](data: RouteBuilderData[_, PathParams, Nothing]) {
  def produces[OUT](implicit entityEncoder: EntityEncoder[F, OUT]): CompletableUnsafeRouteWithOut[F, PathParams, OUT] =
    new CompletableUnsafeRouteWithOut[F, PathParams, OUT](
      data = new RouteBuilderData[Nothing, PathParams, Nothing](
        route = data.route,
        queryParams = data.queryParams,
        authorization = (_, _) => new AlwaysAllow[Nothing]
      ),
      entityEncoder = entityEncoder
    )
}

trait Route[F[_]] {
  def key: List[SimpleLinx]
  def method: Method
  def compile: CompilableRoute[F]
}

case class CompilableRoute[F[_]: Monad](pathMatcher: PartialFunction[String, Directive[F, Any]], methodMap: Map[Method, Any => ResponseDirective[F]]) {
  def merge(other: CompilableRoute[F]): CompilableRoute[F] = {
    this.copy(
      methodMap = this.methodMap ++ other.methodMap
    )
  }

  def compile: PartialFunction[String, ResponseDirective[F]] = {
    def allow(methods: List[Method]): Allow = Allow(methods.head, methods.tail: _*)

    pathMatcher.andThen { pathParamsDir =>
      {
        for {
          req        <- Directive.request
          pathParams <- pathParamsDir
          res <- if (methodMap.isDefinedAt(req.method)) methodMap(req.method)(pathParams)
                else Directive.error(Response[F](Status.MethodNotAllowed).putHeaders(allow(methodMap.keySet.toList)))
        } yield {
          res
        }
      }
    }
  }
}

case class UnsafeGetRoute[F[_]: Monad, PathParams <: HList](key: List[SimpleLinx],
                                                            route: HLinx[PathParams],
                                                            routePf: PartialFunction[String, Directive[F, PathParams]],
                                                            method: Method,
                                                            f: PathParams => ResponseDirective[F])
    extends Route[F] {
  override def compile: CompilableRoute[F] = {
    CompilableRoute(
      pathMatcher = routePf.asInstanceOf[PartialFunction[String, Directive[F, Any]]],
      methodMap = Map(method -> f.asInstanceOf[Any => ResponseDirective[F]])
    )
  }
}

class CompletableUnsafeRouteWithOut[F[_]: Monad, PathParams <: HList, OUT](data: RouteBuilderData[Nothing, PathParams, Nothing],
                                                                           entityEncoder: EntityEncoder[F, OUT]) {
  def consumes[IN](implicit entityDecoder: EntityDecoder[F, IN]): CompletableUnsafeRouteWithInAndOut[F, PathParams, OUT, IN] =
    new CompletableUnsafeRouteWithInAndOut[F, PathParams, OUT, IN](
      data = new RouteBuilderData[IN, PathParams, Nothing](
        route = data.route,
        queryParams = data.queryParams,
        authorization = (_, _) => new AlwaysAllow[Nothing]
      ),
      entityEncoder,
      entityDecoder
    )

  def GET(f: PathParams => Directive[F, OUT]): UnsafeGetRoute[F, PathParams] = {
    UnsafeGetRoute[F, PathParams](
      key = data.route.toSimple.reverse,
      route = data.route,
      routePf = Util.createPf(data.route),
      method = Method.GET,
      f = (pp: PathParams) =>
        f(pp).map { out =>
          Response[F]()
            .withEntity(out)(entityEncoder)
      }
    )
  }
}

object Util {
  def createPf[F[_]: Monad, PathParams <: HList](route: HLinx[PathParams]): PartialFunction[String, Directive[F, PathParams]] =
    new PartialFunction[String, Directive[F, PathParams]] {
      override def isDefinedAt(x: String): Boolean = route.capture(x).isDefined

      override def apply(v1: String): Directive[F, PathParams] = {
        val value: Either[String, PathParams] = route.capture(v1).get

        value match {
          case Left(errorMsg) =>
            Directive.error(
              Response(Status.BadRequest)
                .withEntity(errorMsg)
            )

          case Right(params) =>
            Directive.success(params)

        }
      }
    }

}

case class UnsafePostRoute[F[_]: Monad, IN, OUT, PathParams <: HList](key: List[SimpleLinx],
                                                                      route: HLinx[PathParams],
                                                                      routePf: PartialFunction[String, Directive[F, PathParams]],
                                                                      method: Method,
                                                                      entityDecoder: EntityDecoder[F, IN],
                                                                      f: (IN, PathParams) => ResponseDirective[F])
    extends Route[F]
    with RequestDirectives[F] {

  override def compile: CompilableRoute[F] = {
    implicit val dec: EntityDecoder[F, IN] = entityDecoder
    val fm: Any => ResponseDirective[F] = { (pp) =>
      {
        for {
          in  <- request.bodyAs[F, IN]
          res <- f(in, pp.asInstanceOf[PathParams])
        } yield { res }
      }
    }

    CompilableRoute(
      pathMatcher = routePf.asInstanceOf[PartialFunction[String, Directive[F, Any]]],
      methodMap = Map(method -> fm)
    )
  }
}

class CompletableUnsafeRouteWithInAndOut[F[_]: Monad, PathParams <: HList, OUT, IN](data: RouteBuilderData[IN, PathParams, Nothing],
                                                                                    entityEncoder: EntityEncoder[F, OUT],
                                                                                    entityDecoder: EntityDecoder[F, IN]) {
  def POST(f: (IN, PathParams) => Directive[F, OUT]): UnsafePostRoute[F, IN, OUT, PathParams] = {
    UnsafePostRoute(
      key = data.route.toSimple.reverse,
      route = data.route,
      routePf = Util.createPf[F, PathParams](data.route),
      method = Method.POST,
      entityDecoder = entityDecoder,
      f = { (in, pp) =>
        for {
          res <- f(in, pp)
        } yield {
          Response[F]().withEntity(res)(entityEncoder)
        }
      }
    )
  }
}

class Safe[F[_], USER <: AuthenticatedUser[_, _]] {
  def route[PathParams <: HList](path: HLinx[PathParams]): SafeRoute[F, USER, PathParams] =
    new SafeRoute[F, USER, PathParams](RouteBuilderData[Nothing, PathParams, USER](path))
}

class SafeRoute[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList](data: RouteBuilderData[_, PathParams, USER]) {
  def queryParams[A](param: QueryParam[A]): SafeRoute[F, USER, PathParams] = queryParams(param ::: HNil)

  def queryParams[A <: HList](params: A): SafeRoute[F, USER, PathParams] = new SafeRoute[F, USER, PathParams](data.copy(queryParams = params))

  def consumes[IN]: SafeRouteWithIn[F, USER, PathParams, IN] =
    new SafeRouteWithIn[F, USER, PathParams, IN](RouteBuilderData[IN, PathParams, USER](data.route, data.queryParams))

  def produces[OUT]: SafeRouteWithOut[F, USER, PathParams, OUT] = ???

  //new SafeRouteWithOut[F, USER, PathParams, OUT](data.copy(produces = mediaType.toList))
}

class SafeRouteWithIn[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN](data: RouteBuilderData[IN, PathParams, USER]) {
  def produces[OUT]: SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] =
    new SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT](data)
}

class SafeRouteWithOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, OUT](data: RouteBuilderData[Nothing, PathParams, USER]) {
  def consumes[IN]: SafeRouteWithInAndOut[F, USER, PathParams, IN, OUT] = ???

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
      authorization = data.authorization,
      f = (user, _, pp) => f(user, pp)
    )
}

class CompletableSafeRouteWithInAndOut[F[_], USER <: AuthenticatedUser[_, _], PathParams <: HList, IN, OUT](data: RouteBuilderData[IN, PathParams, USER]) {
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

  case class HasRole(role: String) extends SecurityPredicate[MyAuthenticatedUser](userProfile => userProfile.profile.roles.contains(role))

  case class HasAccessToFeature(feature: String) extends SecurityPredicate[MyAuthenticatedUser](userProfile => userProfile.profile.roles.contains(feature))

  case class BodyContains(content: String, myRequest: MyRequest) extends SecurityPredicate[MyAuthenticatedUser](userProfile => myRequest.in.contains(content))

  case class MyResponse(result: String)

  case class MyRequest(in: String)

  val unsecurity = Unsecurity[IO, MyAuthenticatedUser]

  val aRoute =
    unsecurity.safe
      .route(Root / "aRequest" / param[Int]("intParam") / param[String]("stringParam"))
      .consumes[MyRequest] //application.json by magic somehow makes sure there is an decoder
      .produces[MyResponse] //application.json by magic somehow makes sure there is an encoder
      .authorization((myRequest, pathParams) => HasRole("admin") || HasAccessToFeature("myFeature") && BodyContains("kaare", myRequest))
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
