package io.unsecurity

object Unsecurity {
  def safe: Safe = ???
  def unsafe: Unsafe = ???
}

class Route(
             path: String,
             queryParams: List[String],
             accepts: List[String],
             produces: List[String],
             authorization: Option[String],
             method: String,
             f: () => String
           )

class Unsafe {
  def route(path: String) = ???
}

class Safe {
  def route(path: String): SafeRoute = ???
}

class SafeRoute(
                 path: String,
                 queryParams: List[String],
                 accepts: List[String],
                 produces: List[String],
                 authorization: Option[String]
               ) extends Safe {
  def queryParams(param: String): SafeRoute = ???
  def accepts(mediaType: String*): SafeRoute = ???
  def produces(mediaType: String*): SafeRoute = ???
  def authorization[A](authRule: SecurityPredicate[A]): SafeRoute = ???
  def GET(f: () => String): Route = ???
}

class SecurityPredicate[-A](predicate: A => Boolean) {
  def ||[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) || other(a))

  def &&[B <: A](other: SecurityPredicate[B]) = new SecurityPredicate[B](a => predicate(a) && other(a))

  def unary_!(): SecurityPredicate[A] = new SecurityPredicate[A](a => !predicate(a))

  def apply(a: A) = predicate(a)
}

object Test {
  private val unsecurity: Route = Unsecurity.safe
    .route("")
    .GET { () =>
      ""
    }
}

/*
unsecurity
    .safe|unsafe
    .route("root" / 'id[String])
    .queryParams?(param[List[String]("names)])
    .consumes?(if requesttype with body)[MyRequest](application/json, appliation/xml)
    .produces[MyResponse](application/json)
    .authorization?((HasRole("admin) && HasAcessToParam("knut")))
    .POST{ (body: BalleRequest, knut: Int, names: List[String]) =>
      //body
    }
 */

