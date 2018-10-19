package io.unsecurity


object Unsecurity {

  def safe:SafeRoute = ???
  def unsafe: UnsafeRoute = ???

}

class Route {
  def route(path:String) = ???
}

class UnsafeRoute extends Route {

}

class SafeRoute extends Route {

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

