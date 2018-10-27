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

//  test("match /api/fjon") {
//    val link = Root / "api" / "fjon"
//
//    assert(link.matches("/api/fjon")._1)
//  }
//
//  test("matching variable") {
//    val link = Root / "api" / Param[String]("var1")
//
//    assert(link.matches("/api/fjon")._1)
//  }
}
