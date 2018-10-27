package io.unsecurity.hlinx

object HLinx {

  sealed trait HList
  final class HNil extends HList {
    def ::[T](v: T): HCons[T, HNil] = HCons(v, this)
  }
  val HNil: HNil = new HNil

  case class HCons[H, T <: HList](head: H, tail: T) extends HList {
    def ::[V](v: V): HCons[V, H :: T] = HCons(v, this)
  }

  type ::[H, T <: HList] = HCons[H, T]

  case class Param[A: ParamConverter](name: String)

  sealed trait LinkFragment[A <: HList] {
    def matches(s: String): (Boolean, Vector[String])

    def internalMatches(fs: Vector[String]): (Boolean, Vector[String])

    protected def split(s: String): Vector[String] =
      s.split("/").toVector.filter(_.nonEmpty)
  }

  val Root = StaticFragment(Vector.empty)

  case class StaticFragment(fragments: Vector[String]) extends LinkFragment[HNil] {
    def /(s: String): StaticFragment =
      StaticFragment(fragments ++ split(s))

    def /[A](p: Param[A]): VarFragment[A, HNil] =
      VarFragment(this, Vector.empty, p)

    def matches(s: String): (Boolean, Vector[String]) = {
      internalMatches(split(s))
    }

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) = {

      val g = fs zip fragments

      val isMatch = g.forall { case (a, b) => a == b }
      (isMatch, if (isMatch) fs.drop(g.size) else Vector.empty)
    }

  }

  case class VarFragment[A, B <: HList](parent: LinkFragment[B], static: Vector[String], param: Param[A]) extends LinkFragment[A :: B] {
    def /(s: String): VarFragment[A, B] =
      VarFragment(parent, static ++ s.split("/"), param)

    def /[C](p: Param[C]): VarFragment[C, A :: B] =
      VarFragment(this, Vector.empty, p)

    override def matches(s: String): (Boolean, Vector[String]) = {
      val fs = split(s)

      val (parentMatch, rest): (Boolean, Vector[String]) =
        parent.internalMatches(fs)

      println(parentMatch)
      println(rest)
      ???
    }

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) = ???
  }
}
