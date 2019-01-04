import io.unsecurity.AuthenticatedUser
import org.scalatest.FunSuite
class SecurityPredicateTest extends FunSuite {

  test("Combining predicates with context using || should yield correct contezt "){
    class MyProfile
    case class MyUser(id: String, profile: MyProfile) extends AuthenticatedUser[String, MyProfile]
  }
}
