package io.unsecurity

import no.scalabin.http4s.directives.Directive

trait SecurityContext[F[_], U1, U2] {
  def authenticate: Directive[F, U1]
  def xsrfCheck: Directive[F, String]
  def rateLimitCheck(authenticatedIdentity: U1): Directive[F, Int]
  def transformUser(u: U1): Option[U2]
}
