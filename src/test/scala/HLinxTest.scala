import io.unsecurity.hlinx.HLinx._

import org.scalatest.FunSuite

class HLinxTest extends FunSuite {

  test("match /api") {
    val link = Root / "api"

    assert(link.matches("/api")._1)
  }

  test("match /api/fjon") {
    val link = Root / "api" / "fjon"

    assert(link.matches("/api/fjon")._1)
  }

  test("matching variable") {
    val link = Root / "api" / Param[String]("var1")

    assert(link.matches("/api/fjon")._1)
  }
}
