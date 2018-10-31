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

  case class Param[A: ParamConverter](name: String) {
    def convert(s: String): Either[String, A] =
      implicitly[ParamConverter[A]].convert(s)
  }

  sealed trait Segment
  case class Static(name: String) extends Segment
  case object Var                 extends Segment

  def splitPath(path: String): Vector[String] =
    path.split("/").toVector.filter(_.nonEmpty)

  sealed trait LinkFragment[A <: HList] {
    val didNotMatch: (Boolean, Vector[String]) = (false, Vector.empty[String])

    def staticFragments: Vector[String]

    // TODO rename to definedAt
    final def matches(s: String): Boolean = {
      val (parentMatch, leftovers) = internalMatches(splitPath(s))
      parentMatch && leftovers.isEmpty
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
      StaticFragment(staticFragments ++ splitPath(s))

    def /[A](p: Param[A]): VarFragment[A, HNil] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) =
      matchesStatic(fs)

    override def toSegments: Vector[Segment] = staticFragments.map(Static)
  }

  case class VarFragment[A, B <: HList](parent: LinkFragment[B], staticFragments: Vector[String], param: Param[A])
      extends LinkFragment[A :: B] {
    def /(s: String): VarFragment[A, B] =
      this.copy(staticFragments = staticFragments ++ splitPath(s))

    def /[C](p: Param[C]): VarFragment[C, A :: B] =
      VarFragment(this, Vector.empty, p)

    override def internalMatches(fs: Vector[String]): (Boolean, Vector[String]) = {
      val (parentMatches, leftovers) = parent.internalMatches(fs)

      if (parentMatches && leftovers.nonEmpty) matchesStatic(leftovers.tail)
      else didNotMatch
    }

    override def toSegments: Vector[Segment] = {
      (parent.toSegments :+ Var) ++ staticFragments.map(Static)
    }
  }

  implicit class S0(private val lf: LinkFragment[HNil]) extends AnyVal {
    def capture(path: String): Option[Unit] = {
      capture(splitPath(path)).flatMap {
        case Vector() => Some(())
        case _        => None
      }
    }

    def capture(pathFragments: Vector[String]): Option[Vector[String]] = {
      if (pathFragments.startsWith(lf.staticFragments))
        Some(pathFragments.drop(lf.staticFragments.size))
      else None
    }
  }

  implicit class S1[A](private val lf: LinkFragment[A :: HNil]) extends AnyVal {
    def capture(path: String): Option[Either[String, A]] = {
      capture(splitPath(path)).flatMap {
        case (paramA, Vector()) => Some(paramA)
        case _                  => None
      }
    }

    def capture(pathFragments: Vector[String]): Option[(Either[String, A], Vector[String])] = {
      val vf: VarFragment[A, HNil] = lf.asInstanceOf[VarFragment[A, HNil]]

      for {
        parentLeftovers <- vf.parent.capture(pathFragments)
        thisResult <- parentLeftovers match {
                       case head +: tail if tail.startsWith(vf.staticFragments) =>
                         Some((vf.param.convert(head), tail.drop(vf.staticFragments.size)))
                       case _ => None
                     }
      } yield thisResult
    }
  }

  implicit class S2[A, B](private val lf: VarFragment[A, B :: HNil]) extends AnyVal {
    def capture(path: String): Option[(Either[String, B], Either[String, A])] = {
      capture(splitPath(path)).flatMap {
        case (paramB, paramA, Vector()) => Some((paramB, paramA))
        case _                          => None
      }
    }

    def capture(pathFragments: Vector[String]): Option[(Either[String, B], Either[String, A], Vector[String])] = {
      val vf = lf.asInstanceOf[VarFragment[A, B :: HNil]]

      for {
        (paramB, parentLeftovers) <- vf.parent.capture(pathFragments)
        (paramA, thisLeftOvers) <- parentLeftovers match {
                                    case head +: tail if tail.startsWith(vf.staticFragments) =>
                                      Some((vf.param.convert(head), tail.drop(vf.staticFragments.size)))
                                    case _ => None
                                  }
      } yield (paramB, paramA, thisLeftOvers)
    }
  }
}
