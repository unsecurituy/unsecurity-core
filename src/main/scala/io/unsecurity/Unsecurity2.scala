package io.unsecurity

import cats.effect.Sync
import io.unsecurity.hlinx.HLinx.{HList, SimpleLinx}
import no.scalabin.http4s.directives.Conditional.ResponseDirective
import no.scalabin.http4s.directives.Directive
import org.http4s.headers.Allow
import org.http4s.{EntityDecoder, EntityEncoder, Method, Response, Status}

class Unsecurity2[F[_]: Sync, U] extends AbstractUnsecurity2[F, U] with UnsecurityOps[F] {

  def sc: SecurityContext[F, U] = ???

  case class MySecured[C, W](
      key: List[SimpleLinx],
      pathMatcher: PathMatcher[F, Any],
      methodMap: Map[Method, Any => Directive[F, C]],
      entityEncoder: EntityEncoder[F, W]
  ) extends Secured[C, W] {
    override def authorization(predicate: C => Boolean): Completable[C, W] = {
      MyCompletable(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = {
          methodMap.mapValues(
            a2dc =>
              a2dc.andThen(
                dc =>
                  dc.flatMap(
                    c =>
                      if (predicate(c)) Directive.success(c)
                      else
                        Directive.error(
                          Response[F]()
                            .withStatus(Status.Forbidden)
                      )
                )
            ))
        },
        entityEncoder = entityEncoder
      )
    }
    override def resolve[C2](f: C => F[C2]): Secured[C2, W] = {
      MySecured(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues(
          a2dc =>
            a2dc.andThen(
              dc =>
                dc.map { c =>
                  val value: F[C2] = f(c)
                  ???
              }
          )
        ),
        entityEncoder = entityEncoder
      )
    }
    override def run(f: C => Directive[F, W]): Complete = {
      MyComplete(
        key = key,
        pathMatcher = pathMatcher,
        methodMap = methodMap.mapValues(
          a2dc =>
            a2dc.andThen(
              dc =>
                for {
                  c <- dc
                  w <- f(c)
                } yield {
                  Response[F]()
                    .withEntity(w)(entityEncoder)
              }
          )
        )
      )
    }
  }

  override def secure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Secured[(P, R, U), W] = {
    MySecured[(P, R, U), W](
      key = endpoint.path.toSimple.reverse,
      pathMatcher = createPathMatcher(endpoint.path).asInstanceOf[PathMatcher[F, Any]],
      methodMap = Map(
        endpoint.method -> { pp: P =>
          implicit val entityDecoder: EntityDecoder[F, R] = endpoint.accepts
          for {
            r    <- request.bodyAs[F, R]
            user <- sc.authenticate
          } yield {
            (pp, r, user)
          }
        }.asInstanceOf[Any => Directive[F, (P, R, U)]]
      ),
      entityEncoder = endpoint.produces
    )
  }

  override def unsecure[P <: HList, R, W](endpoint: Endpoint[P, R, W]): Completable[(P, R), W] = {
    MyCompletable[(P, R), W](
      key = endpoint.path.toSimple.reverse,
      pathMatcher = createPathMatcher[F, P](endpoint.path).asInstanceOf[PathMatcher[F, Any]],
      methodMap = Map(
        endpoint.method -> { pp: P =>
          implicit val entityDecoder: EntityDecoder[F, R] = endpoint.accepts
          for {
            r <- request.bodyAs[F, R]
          } yield {
            (pp, r)
          }
        }.asInstanceOf[Any => Directive[F, (P, R)]]
      ),
      entityEncoder = endpoint.produces
    )
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
        methodMap = methodMap.mapValues {
          //noinspection ScalaUnnecessaryParentheses
          a2dc: (Any => Directive[F, C]) =>
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
    override def merge(other: AbstractUnsecurity2[F, U]#Complete): AbstractUnsecurity2[F, U]#Complete = {
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
}
