package io

import cats.Monad
import io.unsecurity.hlinx.HLinx.{HLinx, HList}
import no.scalabin.http4s.directives.Directive
import org.http4s.{Response, Status}

package object unsecurity {
  // TODO this can be moved into Unsecurity2 when Unsecurity is deleted

  type PathMatcher[F[_], A] = PartialFunction[String, Directive[F, A]]

  def createPathMatcher[F[_]: Monad, PathParams <: HList](route: HLinx[PathParams]): PathMatcher[F, PathParams] =
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
