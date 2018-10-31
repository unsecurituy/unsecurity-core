import io.unsecurity.hlinx.HLinx._
import org.scalatest.FunSpec

class HLinxTest extends FunSpec {

  describe("capture") {
    describe("StaticFragment") {
      describe("Root / foo / bar") {
        val link: StaticFragment = Root / "foo" / "bar"

        it("should match equal path") {
          assert(link.capture("/foo/bar").isDefined)
        }

        it("should not match shorter path") {
          assert(link.capture("/foo") === None)
        }

        it("should not match different path") {
          assert(link.capture("foo/baz") === None)
        }

        it("should not match longer path") {
          assert(link.capture("/foo/bar/baz") === None)
        }
      }
    }

    describe("VarFragment") {
      describe("with one param") {
        val link = Root / "foo" / Param[String]("str") / "bar"

        it("should not match shorter path") {
          assert(link.capture("/foo") === None)
        }

        it("should not match longer path") {
          assert(link.capture("/foo/str/bar/longer") === None)
        }

        it("should not match different path") {
          assert(link.capture("/bar/str/foo") === None)
        }

        it("should not match if static part of varfragment doesn't match") {
          assert(link.capture("/foo/str") === None)
        }

        it("should match equal path") {
          assert(link.capture("/foo/str/bar") === Some(Right("str")))
        }
      }

      describe("with two params") {
        val link: LinkFragment[Int :: String :: HNil] =
          Root / "foo" / Param[String]("str") / Param[Int]("int") / "bar"

        it("should not match shorter path") {
          assert(link.capture("/foo") === None)
        }

        it("should not match if static in varfragment doesn't match") {
          assert(link.capture("/foo/str/1/baz") === None)
        }

        it("should match equal path") {
          assert(link.capture("/foo/str/1/bar") === Some((Right("str"), Right(1))))
        }
      }
    }
  }

  describe("overlaps") {
    describe("StaticFragment") {
      describe("Root / foo / bar") {
        val link: StaticFragment = Root / "foo" / "bar"

        it("overlap /foo/bar") {
          assert(link.overlaps(Root / "foo" / "bar"))
        }

        it("do not overlap /foo/bar/baz") {
          assert(!link.overlaps(Root / "foo" / "baz"))
        }

        it("do not overlap /foo/bar/Param[String]") {
          assert(!link.overlaps(Root / "foo" / "bar" / Param[String]("")))
        }

        it("overlap /foo/Param[String)(bar)") {
          assert(link.overlaps(Root / "foo" / Param[String]("bar")))
        }

        it("do not overlap /foo/Param[String)(bar)/baz") {
          assert(!link.overlaps(Root / "foo" / Param[String]("bar") / "baz"))
        }
      }
    }

    describe("VarFragment") {
      describe("Root / Param[String] / bar") {
        val link: VarFragment[String, HNil] = Root / Param[String]("") / "bar"

        it("overlap /foo/bar") {
          assert(link.overlaps(Root / "foo" / "bar"))
        }

        it("overlap /foo/Param[String]") {
          assert(link.overlaps(Root / "foo" / Param[String]("")))
        }

        it("do not overlap /foo/Param[String]/baz") {
          assert(!link.overlaps(Root / "foo" / Param[String]("") / "baz"))
        }
      }
    }
  }
}
