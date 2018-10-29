import io.unsecurity.hlinx.HLinx._
import org.scalatest.FunSpec

class HLinxTest extends FunSpec {

  val r = Root / Param[String]("fisk")

  describe("matches") {
    describe("StaticFragment") {
      describe("Root / foo / bar") {
        val link: StaticFragment = Root / "foo" / "bar"

        it("do not matches /foo") {
          assert(!link.matches("/foo"))
        }

        it("matches /foo/bar") {
          assert(link.matches("/foo/bar"))
        }

        it("matches foo/bar") {
          assert(link.matches("foo/bar"))
        }

        it("do not matches /foo/bar/baz") {
          assert(!link.matches("/foo/bar/baz"))
        }
      }
    }

    describe("VariableFragment") {
      describe("Root / foo / Param[String](bar)") {
        val link: VarFragment[String, HNil] = Root / "foo" / Param[String]("bar")

        it("matches /foo/foo") {
          assert(link.matches("/foo/foo"))
        }

        it("matches /foo/1") {
          assert(link.matches("/foo/1"))
        }

        it("do not matches /foo") {
          assert(!link.matches("/foo"))
        }
      }

      describe("Root / foo / Param[String](bar) / baz") {
        val link: VarFragment[String, HNil] = Root / "foo" / Param[String]("bar") / "baz"

        it("matches /foo/1/baz") {
          assert(link.matches("/foo/1/baz"))
        }

        it("do not matches /foo/1") {
          assert(!link.matches("/foo/1"))
        }

        it("do not matches /foo/1/foo") {
          assert(!link.matches("/foo/1/foo"))
        }

        it("do not matches /foo/1/baz/baz") {
          assert(!link.matches("/foo/1/baz/baz"))
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
          assert(link.overlaps(Root / "foo" / "bar"))
        }

        it("do not overlap /foo/bar/Param[String)(baz)") {
          assert(!link.overlaps(Root / "foo" / "bar" / Param[String]("baz")))
        }

        it("overlap /foo/Param[String)(bar)") {
          assert(link.overlaps(Root / "foo" / Param[String]("bar")))
        }

        it("do not overlap /foo/Param[String)(bar)/baz") {
          assert(!link.overlaps(Root / "foo" / Param[String]("bar") / "baz"))
        }
      }
    }
  }
}
