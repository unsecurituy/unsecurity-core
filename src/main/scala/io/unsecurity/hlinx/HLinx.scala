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

  sealed trait Segment
  case class Static(name: String) extends Segment
  case object Var                 extends Segment

  sealed trait LinkFragment[A <: HList] {
    val didNotMatch = (false, Vector.empty[String])

    def staticFragments: Vector[String]

    final def matches(s: String): Boolean = {
      val (parentMatch, leftOvers) = internalMatches(split(s))
      parentMatch && leftOvers.isEmpty
    }

    def internalMatches(fs: Vector[String]): (Boolean, Vector[String])

    def matchesStatic(fs: Vector[String]): (Boolean, Vector[String]) = {
      val zipped: Vector[(String, String)] = fs zip staticFragments
      if (zipped.size < staticFragments.size) (false, fs.drop(zipped.size))
      else {
        val isMatch = zipped.forall { case (a, b) => a == b }

        if (isMatch) (true, fs.drop(zipped.size))
        else didNotMatch
      }
    }

    protected def split(s: String): Vector[String] =
      s.split("/").toVector.filter(_.nonEmpty)

    def overlaps[B <: HList](that: LinkFragment[B]): Boolean = {
      val as: Vector[Segment] = toSegments
      val bs: Vector[Segment] = that.toSegments

      if (as.size != bs.size) false
      else {
        (as zip bs).foldLeft(true) {
          case (acc, t) =>
            acc && (t match {
              case (Var, _)               => true
              case (_, Var)               => true
              case (Static(a), Static(b)) => a == b
            })
        }
      }
    }

    def toSegments: Vector[Segment]
  }

  val Root = StaticFragment(Vector.empty)

  case class StaticFragment(staticFragments: Vector[String]) extends LinkFragment[HNil] {
    def /(s: String): StaticFragment =
      StaticFragment(staticFragments ++ split(s))

    def /[A](p: Param[A]): VarFragment[A, HNil] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) =
      matchesStatic(fs)

    def toSegments: Vector[Segment] = staticFragments.map(Static)
  }

  case class VarFragment[A, B <: HList](parent: LinkFragment[B], staticFragments: Vector[String], param: Param[A])
      extends LinkFragment[A :: B] {
    def /(s: String): VarFragment[A, B] =
      VarFragment(parent, staticFragments ++ s.split("/"), param)

    def /[C](p: Param[C]): VarFragment[C, A :: B] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) = {
      val (parentMatches, leftOvers) = parent.internalMatches(fs)

      if (parentMatches && leftOvers.nonEmpty) matchesStatic(leftOvers.tail)
      else didNotMatch
    }

    override def toSegments: Vector[Segment] = {
      (parent.toSegments :+ Var) ++ staticFragments.map(Static)
    }
  }
}
