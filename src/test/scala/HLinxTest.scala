import io.unsecurity.hlinx.HLinx._
import org.scalatest.FunSpec

class HLinxTest extends FunSpec {

  describe("StaticFragment") {
    describe("Root / foo / bar") {
      val link: StaticFragment = Root / "foo" / "bar"

      it("should not match /foo") {
        assert(!link.matches("/foo"))
      }

      it("should match /foo/bar") {
        assert(link.matches("/foo/bar"))
      }

      it("should match foo/bar") {
        assert(link.matches("foo/bar"))
      }

      it("should not match /foo/bar/baz") {
        assert(!link.matches("/foo/bar/baz"))
      }
    }
  }

  describe("VariableFragment") {
    describe("Root / foo / Param[String](bar)") {
      val link: VarFragment[String, HNil] = Root / "foo" / Param[String]("bar")

      it("should match /foo/foo") {
        assert(link.matches("/foo/foo"))
      }

      it("should match /foo/1") {
        assert(link.matches("/foo/1"))
      }

      it("should not match /foo") {
        assert(!link.matches("/foo"))
      }
    }

    describe("Root / foo / Param[String](bar) / baz") {
      val link: VarFragment[String, HNil] = Root / "foo" / Param[String]("bar") / "baz"

      it("should match /foo/1/baz") {
        assert(link.matches("/foo/1/baz"))
      }

      it("should not match /foo/1") {
        assert(!link.matches("/foo/1"))
      }

      it("should not match /foo/1/foo") {
        assert(!link.matches("/foo/1/foo"))
      }

      it("should not match /foo/1/baz/baz") {
        assert(!link.matches("/foo/1/baz/baz"))
      }
    }
  }
}
