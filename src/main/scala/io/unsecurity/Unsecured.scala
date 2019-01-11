package io.unsecurity
import cats.Monad
import io.unsecurity.hlinx.HLinx.{HLinx, HList, HNil, SimpleLinx}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, RequestDirectives}
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, Method, Response, Status}

object Unsecured {

  trait Route[F[_]] {
    def key: List[SimpleLinx]
    def method: Method
    def compile: CompilableRoute[F]
  }

  class UnsecuredRoute[F[_]: Monad, PathParams <: HList](route: HLinx[PathParams]) {
    def producesJson[OUT](
        implicit entityEncoder: EntityEncoder[F, OUT]): CompletableUnsecuredRouteWithOut[F, PathParams, OUT] =
      new CompletableUnsecuredRouteWithOut[F, PathParams, OUT](
        route,
        entityEncoder = entityEncoder
      )
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

  case class UnsecuredGetRoute[F[_]: Monad, PathParams <: HList](
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

  class CompletableUnsecuredRouteWithOut[F[_]: Monad, PathParams <: HList, OUT](route: HLinx[PathParams],
                                                                                entityEncoder: EntityEncoder[F, OUT]) {
    def consumesJson[IN](
        implicit entityDecoder: EntityDecoder[F, IN]): CompletableUnsafeRouteWithInAndOut[F, PathParams, OUT, IN] =
      new CompletableUnsafeRouteWithInAndOut[F, PathParams, OUT, IN](
        route = route,
        entityEncoder = entityEncoder,
        entityDecoder = entityDecoder
      )

    def GET(f: PathParams => Directive[F, OUT]): UnsecuredGetRoute[F, PathParams] = {
      UnsecuredGetRoute[F, PathParams](
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

  case class UnsafePostRoute[F[_]: Monad, IN, OUT, PathParams <: HList](
      key: List[SimpleLinx],
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

  class CompletableUnsafeRouteWithInAndOut[F[_]: Monad, PathParams <: HList, OUT, IN](
      route: HLinx[PathParams],
      entityEncoder: EntityEncoder[F, OUT],
      entityDecoder: EntityDecoder[F, IN]) {
    def POST(f: (IN, PathParams) => Directive[F, OUT]): UnsafePostRoute[F, IN, OUT, PathParams] = {
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
