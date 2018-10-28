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
    val didNotMatch   = (false, Vector.empty[String])
    val didNotOverlap = (false, Vector.empty, Vector.empty)

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

        if (isMatch) (true, fs.drop(zipped.size))
        else didNotMatch
      }
    }

    protected def split(s: String): Vector[String] =
      s.split("/").toVector.filter(_.nonEmpty)
  }

  val Root = StaticFragment(Vector.empty)

  case class StaticFragment(staticFragments: Vector[String]) extends LinkFragment[HNil] {
    def overlaps(fragment: StaticFragment): Boolean = {
      val (doOverlap, thisLeftOvers, thatLeftOvers) = iol(fragment)

      doOverlap && thisLeftOvers.isEmpty && thatLeftOvers.isEmpty
    }

    def overlaps[A, B <: HList](fragment: VarFragment[A, B]): Boolean = {
      val (doOverlap, thisLeftOvers, thatLeftOvers) = iol(fragment)

      doOverlap && thisLeftOvers.isEmpty && thatLeftOvers.isEmpty
    }

    def iol[A <: HList](fragment: LinkFragment[A]): (Boolean, Vector[String], Vector[String]) = {
      fragment match {
        case sf: StaticFragment    => internalOverlaps(sf)
        case vf: VarFragment[_, A] => internalOverlaps(vf)
      }
    }

    def internalOverlaps(fragment: StaticFragment): (Boolean, Vector[String], Vector[String]) = {
      if (staticFragments.size == fragment.staticFragments.size) {
        if (staticFragments == fragment.staticFragments) {
          (true, Vector.empty, Vector.empty)
        } else didNotOverlap
      } else if (staticFragments.size > fragment.staticFragments.size) {
        if (staticFragments.startsWith(fragment.staticFragments)) {
          (true, staticFragments.drop(fragment.staticFragments.size), Vector.empty)
        } else {
          didNotOverlap
        }
      } else {
        if (fragment.staticFragments.startsWith(staticFragments)) {
          (true, Vector(), fragment.staticFragments.drop(staticFragments.size))
        } else {
          didNotOverlap
        }
      }
    }

    // Static(/foo/bar)
    // Static(/foo) / Var(Param[String]) / "jodle"
    def internalOverlaps[A, B <: HList](fragment: VarFragment[A, B]): (Boolean, Vector[String], Vector[String]) = {
      val tuple = iol(fragment.parent)
      tuple match {
        case (false, _, _)                 => didNotOverlap
        case (true, Vector(), _)           => didNotOverlap
        case (true, h +: t, thatLeftOvers) => (true, t, thatLeftOvers ++ fragment.staticFragments)
      }
    }

    def /(s: String): StaticFragment =
      StaticFragment(staticFragments ++ split(s))

    def /[A](p: Param[A]): VarFragment[A, HNil] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) =
      matchesStatic(fs)
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
  }
}
