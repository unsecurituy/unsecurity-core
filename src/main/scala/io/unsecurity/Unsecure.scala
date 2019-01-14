package io.unsecurity
import cats.Monad
import cats.effect.Sync
import io.unsecurity.hlinx.HLinx.{:::, HCons, HLinx, HList, HNil, SimpleLinx}
import io.unsecurity.hlinx.{HLinx, QueryParam, QueryParamConverter}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, Plan, RequestDirectives}
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Response, Status}

object Unsecure {

  class Endpoints[F[_]: Sync](rs: List[Endpoint[F]] = List.empty) {
    def addRoute[PathParams <: HList](route: EndpointWithPath[F, PathParams]): Endpoints[F] = {
      rs.find(
          r => r.overlaps(route.path).isDefined && route.method == r.method
        )
        .foreach { overlapping =>
          throw new RuntimeException(
            s"overlapping method/path: ${overlapping.method.name} ${overlapping.key.mkString("/")}")
        }

      new Endpoints(route :: rs)
    }

    def toHttpRoutes: HttpRoutes[F] = {
      val linxesToList: Map[List[SimpleLinx], List[Endpoint[F]]] = rs.groupBy(_.key)
      val mergedRoutes: List[PathMatcher[F, Response[F]]] =
        linxesToList.mapValues(rs => rs.map(_.compile).reduce(_ merge _)).values.toList.map(_.compile)

      val reducedRoutes: PathMatcher[F, Response[F]] = mergedRoutes.reduce(_ orElse _)

      val PathMapping = Plan[F]().PathMapping

      val service: HttpRoutes[F] = HttpRoutes.of[F](
        PathMapping(reducedRoutes)
      )

      service
    }
  }

  trait Endpoint[F[_]] {
    def key: List[SimpleLinx]
    def method: Method
    def compile: CompilableEndpoint[F]
    def overlaps[OtherPathParams <: HList](otherPath: HLinx[OtherPathParams]): Option[String]
  }

  trait EndpointWithPath[F[_], PathParams <: HList] extends Endpoint[F] {
    def path: HLinx[PathParams]

    override def overlaps[OtherPathParams <: HList](otherPath: HLinx[OtherPathParams]): Option[String] = {
      if (path.overlaps(otherPath))
        Some(path.toString)
      else
        None
    }

  }

  case class QueryParams[T <: HList] private (params: T = HNil) {
    def &[H](queryParam: QueryParam[H]): QueryParams[QueryParam[H] ::: T] =
      this.copy(HCons(queryParam, params))
  }
  object QueryParams {
    val empty = QueryParams(HNil)
    def of[A: QueryParamConverter](name: String): QueryParams[HLinx.:::[QueryParam[A], HNil]] =
      QueryParams(QueryParam[A](name) ::: HNil)
  }

  case class UnsecureEndpoint[F[_]: Monad, PathParams <: HList, QParams <: HList](
      route: HLinx[PathParams],
      queryParams: QueryParams[QParams]) {

    // TODO se på phantomTypes
    def queryParams[QP <: HList](qParams: QueryParams[QP]): UnsecureEndpoint[F, PathParams, QP] = {
      copy(queryParams = qParams)
    }

    def producesJson[W](implicit entityEncoder: EntityEncoder[F, W]): UnsecureEndpointW[F, PathParams, W] =
      new UnsecureEndpointW[F, PathParams, W](
        route,
        entityEncoder = entityEncoder
      )
  }

  class UnsecureEndpointW[F[_]: Monad, PathParams <: HList, W](route: HLinx[PathParams],
                                                               entityEncoder: EntityEncoder[F, W]) {
    def consumesJson[R](implicit entityDecoder: EntityDecoder[F, R]): UnsecureEndpointRW[F, PathParams, R, W] =
      new UnsecureEndpointRW[F, PathParams, R, W](
        route = route,
        entityEncoder = entityEncoder,
        entityDecoder = entityDecoder
      )

    def GET(f: PathParams => Directive[F, W]): UnsecureGetEndpoint[F, PathParams] = {
      UnsecureGetEndpoint[F, PathParams](
        key = route.toSimple.reverse,
        path = route,
        pathMatcher = createPathMatcher(route),
        method = Method.GET,
        f = (pp: PathParams) =>
          f(pp).map { out =>
            Response[F]()
              .withEntity(out)(entityEncoder)
        }
      )
    }
  }

  class UnsecureEndpointRW[F[_]: Monad, PathParams <: HList, R, W](route: HLinx[PathParams],
                                                                   entityEncoder: EntityEncoder[F, W],
                                                                   entityDecoder: EntityDecoder[F, R]) {
    def POST(f: (R, PathParams) => Directive[F, W]): UnsecurePostEndpointRW[F, PathParams, R, W] = {
      UnsecurePostEndpointRW(
        key = route.toSimple.reverse,
        path = route,
        pathMatcher = createPathMatcher[F, PathParams](route),
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

  case class UnsecurePostEndpointRW[F[_]: Monad, PathParams <: HList, R, W](
      key: List[SimpleLinx],
      path: HLinx[PathParams],
      pathMatcher: PathMatcher[F, PathParams],
      method: Method,
      entityDecoder: EntityDecoder[F, R],
      f: (R, PathParams) => ResponseDirective[F])
      extends EndpointWithPath[F, PathParams]
      with RequestDirectives[F] {

    override def compile: CompilableEndpoint[F] = {
      implicit val dec: EntityDecoder[F, R] = entityDecoder
      val fm: Any => ResponseDirective[F] = { (pp) =>
        {
          for {
            in  <- request.bodyAs[F, R]
            res <- f(in, pp.asInstanceOf[PathParams])
          } yield { res }
        }
      }

      CompilableEndpoint(
        pathMatcher = pathMatcher.asInstanceOf[PathMatcher[F, Any]],
        methodMap = Map(method -> fm)
      )
    }
  }

  case class CompilableEndpoint[F[_]: Monad](pathMatcher: PathMatcher[F, Any],
                                             methodMap: Map[Method, Any => ResponseDirective[F]]) {
    def merge(other: CompilableEndpoint[F]): CompilableEndpoint[F] = {
      this.copy(
        methodMap = this.methodMap ++ other.methodMap
      )
    }

    def compile: PartialFunction[String, ResponseDirective[F]] = {
      def allow(methods: List[Method]): Allow = Allow(methods.head, methods.tail: _*)

      pathMatcher.andThen { pathParamsDir =>
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

  case class UnsecureGetEndpoint[F[_]: Monad, PathParams <: HList](
      key: List[SimpleLinx],
      path: HLinx[PathParams],
      pathMatcher: PathMatcher[F, PathParams],
      method: Method,
      f: PathParams => ResponseDirective[F])
      extends EndpointWithPath[F, PathParams] {
    override def compile: CompilableEndpoint[F] = {
      CompilableEndpoint(
        pathMatcher = pathMatcher.asInstanceOf[PathMatcher[F, Any]],
        methodMap = Map(method -> f.asInstanceOf[Any => ResponseDirective[F]])
      )
    }
  }

  type PathMatcher[F[_], A] = PartialFunction[String, Directive[F, A]]

  def createPathMatcher[F[_]: Monad, PathParams <: HList](
      route: HLinx[PathParams]): PathMatcher[F, PathParams] =
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
