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
    def staticFragments: Vector[String]

    def matches(s: String): Boolean = {
      val (parentMatch, leftOvers) = internalMatches(split(s))
      parentMatch && leftOvers.isEmpty
    }

    def internalMatches(fs: Vector[String]): (Boolean, Vector[String])

    def matchesStatic(fs: Vector[String]): (Boolean, Vector[String]) = {
      val zipped: Vector[(String, String)] = fs zip staticFragments
      if (zipped.size < staticFragments.size) (false, fs.drop(zipped.size))
      else {
        val isMatch = zipped.forall { case (a, b) => a == b }
        (isMatch, if (isMatch) fs.drop(zipped.size) else Vector.empty)
      }
    }

    protected def split(s: String): Vector[String] =
      s.split("/").toVector.filter(_.nonEmpty)
  }

  val Root = StaticFragment(Vector.empty)

  case class StaticFragment(staticFragments: Vector[String]) extends LinkFragment[HNil] {
    def /(s: String): StaticFragment =
      StaticFragment(staticFragments ++ split(s))

    def /[A](p: Param[A]): VarFragment[A, HNil] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) =
      matchesStatic(fs)
  }

  case class VarFragment[A, B <: HList](parent: LinkFragment[B], staticFragments: Vector[String], param: Param[A]) extends LinkFragment[A :: B] {
    def /(s: String): VarFragment[A, B] =
      VarFragment(parent, staticFragments ++ s.split("/"), param)

    def /[C](p: Param[C]): VarFragment[C, A :: B] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) = {
      val (parentMatches, leftOvers) = parent.internalMatches(fs)

      if (parentMatches) {
        if (leftOvers.nonEmpty) {
          matchesStatic(leftOvers.drop(1))
        } else {
          (false, Vector.empty)
        }
      } else {
        (false, Vector.empty)
      }
    }
  }
}
