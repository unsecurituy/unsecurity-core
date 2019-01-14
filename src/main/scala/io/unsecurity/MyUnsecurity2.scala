package io.unsecurity

import cats.effect.Sync
import io.unsecurity.hlinx.HLinx.{HList, SimpleLinx}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.{Directive, Plan}
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, HttpRoutes, Method, Response, Status}

class MyUnsecurity2[F[_]: Sync] extends Unsecurity2[F] with UnsecurityOps[F] {

  class UnsecureAuthenticator extends Authenticator[Unit] {
    override def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Secured[(P, R, Unit), W] = ???
    override def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W] = {
      MyCompletable[(P, R), W](
        key = endpoint.path.toSimple.reverse,
        pathMatcher = createPathMatcher[F, P](endpoint.path).asInstanceOf[PathMatcher[F, Any]],
        methodMap = Map(
          endpoint.method -> { pp: P =>
            implicit val entityDecoder: EntityDecoder[F, R] = endpoint.read
            for {
              r <- request.bodyAs[F, R]
            } yield {
              (pp, r)
            }
          }.asInstanceOf[Any => Directive[F, (P, R)]]
        ),
        entityEncoder = endpoint.write
      )
    }
  }

  case class MyCompletable[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Completable[C, W] {
    override def run(f: C => Directive[F, W]): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues { a2dc: (Any => Directive[F, C]) =>
          a2dc.andThen { dc =>
            for {
              c <- dc
              w <- f(c)
            } yield {
              Response[F]()
                .withEntity(w)(entityEncoder)
            }
          }
        }
      )
    }

    override def resolve[C2](f: C => F[C2]): Completable[C2, W] = ???
  }

  case class MyComplete(
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => ResponseDirective[F]]
  ) extends Complete {
    override def ||(next: Complete): Complete = ???
    override def merge(other: Complete): Complete = {
      this.copy(
        methodMap = this.methodMap ++ other.methodMap
      )
    }
    override def compile: PathMatcher[F, Response[F]] = {
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

  override def toHttpRoutes(endpoints: List[Complete]): HttpRoutes[F] = {
    val linxesToList: Map[List[SimpleLinx], List[Complete]] = endpoints.groupBy(_.key)

    val mergedRoutes: List[Complete] =
      linxesToList.toList.map {
        case (_, groupedEndpoints) => groupedEndpoints.reduce(_ merge _)
      }

    val compiledRoutes: List[PathMatcher[F, Response[F]]] =
      mergedRoutes.map(_.compile)

    val reducedRoutes: PathMatcher[F, Response[F]] = compiledRoutes.reduce(_ orElse _)

    val PathMapping = Plan[F]().PathMapping

    val service: HttpRoutes[F] = HttpRoutes.of[F](
      PathMapping(reducedRoutes)
    )

    service
  }

}
