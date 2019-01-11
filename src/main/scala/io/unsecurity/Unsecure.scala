package io.unsecurity
import cats.Monad
import cats.effect.Sync
import io.unsecurity.hlinx.HLinx.{HLinx, HList, SimpleLinx}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, Plan, RequestDirectives}
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Response, Status}

object Unsecure {

  class Routes[F[_]: Sync](rs: List[Route[F]] = List.empty) {
    def addRoute(route: Route[F]): Routes[F] = {
      rs.find(
          r => route.key == r.key && route.method == r.method
        )
        .foreach { overlapping =>
          throw new RuntimeException(s"overlapping method/path: ${overlapping.method.name} ${overlapping.key.mkString("/")}" )
        }

      new Routes(route :: rs)
    }

    def toHttpRoutes: HttpRoutes[F] = {
      val linxesToList: Map[List[SimpleLinx], List[Route[F]]] = rs.groupBy(_.key)
      val mergedRoutes: List[PartialFunction[String, ResponseDirective[F]]] =
        linxesToList.mapValues(rs => rs.map(_.compile).reduce(_ merge _)).values.toList.map(_.compile)

      val reducedRoutes: PartialFunction[String, ResponseDirective[F]] = mergedRoutes.reduce(_ orElse _)

      val PathMapping = Plan[F]().PathMapping

      val service: HttpRoutes[F] = HttpRoutes.of[F](
        PathMapping(reducedRoutes)
      )

      service
    }
  }

  trait Route[F[_]] {
    def key: List[SimpleLinx]
    def method: Method
    def compile: CompilableRoute[F]
  }

  class UnsecureRoute[F[_]: Monad, PathParams <: HList](route: HLinx[PathParams]) {
    def producesJson[W](implicit entityEncoder: EntityEncoder[F, W]): UnsecureRouteW[F, PathParams, W] =
      new UnsecureRouteW[F, PathParams, W](
        route,
        entityEncoder = entityEncoder
      )
  }

  class UnsecureRouteW[F[_]: Monad, PathParams <: HList, W](route: HLinx[PathParams],
                                                            entityEncoder: EntityEncoder[F, W]) {
    def consumesJson[R](implicit entityDecoder: EntityDecoder[F, R]): UnsecureRouteRW[F, PathParams, R, W] =
      new UnsecureRouteRW[F, PathParams, R, W](
        route = route,
        entityEncoder = entityEncoder,
        entityDecoder = entityDecoder
      )

    def GET(f: PathParams => Directive[F, W]): UnsecureGetRoute[F, PathParams] = {
      UnsecureGetRoute[F, PathParams](
        key = route.toSimple.reverse,
        route = route,
        routePf = Util.createPf(route),
        method = Method.GET,
        f = (pp: PathParams) =>
          f(pp).map { out =>
            Response[F]()
              .withEntity(out)(entityEncoder)
        }
      )
    }
  }

  class UnsecureRouteRW[F[_]: Monad, PathParams <: HList, R, W](route: HLinx[PathParams],
                                                                entityEncoder: EntityEncoder[F, W],
                                                                entityDecoder: EntityDecoder[F, R]) {
    def POST(f: (R, PathParams) => Directive[F, W]): UnsafePostRoute[F, PathParams, R, W] = {
      UnsafePostRoute(
        key = route.toSimple.reverse,
        route = route,
        routePf = Util.createPf[F, PathParams](route),
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

  case class UnsafePostRoute[F[_]: Monad, PathParams <: HList, R, W](
      key: List[SimpleLinx],
      route: HLinx[PathParams],
      routePf: PartialFunction[String, Directive[F, PathParams]],
      method: Method,
      entityDecoder: EntityDecoder[F, R],
      f: (R, PathParams) => ResponseDirective[F])
      extends Route[F]
      with RequestDirectives[F] {

    override def compile: CompilableRoute[F] = {
      implicit val dec: EntityDecoder[F, R] = entityDecoder
      val fm: Any => ResponseDirective[F] = { (pp) =>
        {
          for {
            in  <- request.bodyAs[F, R]
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

  case class CompilableRoute[F[_]: Monad](pathMatcher: PartialFunction[String, Directive[F, Any]],
                                          methodMap: Map[Method, Any => ResponseDirective[F]]) {
    def merge(other: CompilableRoute[F]): CompilableRoute[F] = {
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

  case class UnsecureGetRoute[F[_]: Monad, PathParams <: HList](
      key: List[SimpleLinx],
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

}

object Util {
  def createPf[F[_]: Monad, PathParams <: HList](
      route: HLinx[PathParams]): PartialFunction[String, Directive[F, PathParams]] =
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
