package io.unsecurity

import no.scalabin.http4s.directives.Directive

trait SecurityContext[F[_], U] {
  def authenticate: Directive[F, U]
  def xsrfCheck: Directive[F, String]
  def rateLimitCheck(authenticatedIdentity: U): Directive[F, Int]
}
