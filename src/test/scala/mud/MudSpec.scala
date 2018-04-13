package mud

import org.scalatest._

class MudSpec extends FlatSpec with Matchers {
    "The Utils object" should "check_name" in {
        Utils.check_name("xx") shouldEqual false
        Utils.check_name("xxx") shouldEqual true
        Utils.check_name("xxxxxxxxxxxx") shouldEqual true
        Utils.check_name("xxxxxxxxxxxxx") shouldEqual false
        Utils.check_name("x x") shouldEqual false
        Utils.check_name("xx xxxxxxxxx") shouldEqual false
    }
}
