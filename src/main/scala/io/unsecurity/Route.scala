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

