package mud

import org.scalatest._

import mud._

class MudSpec extends FlatSpec with Matchers {
    "The Utils object" should "check_name" in {
        Utils.check_name("xx") shouldBe false
        Utils.check_name("xxx") shouldBe true
        Utils.check_name("xxxxxxxxxxxx") shouldBe true
        Utils.check_name("xxxxxxxxxxxxx") shouldBe false
        Utils.check_name("x x") shouldBe false
        Utils.check_name("xx xxxxxxxxx") shouldBe false
    }

    "The Help object" should "loadHelpFiles" in {
        val help = new Help("./help/")
        val helpArray = help.loadHelpFiles()
        helpArray foreach { case HelpData(_, _, text) => text.length() > 159 shouldBe true }
        helpArray.map { case HelpData(keyword, _, _) => keyword }.toSet shouldBe Set("GREETING", "GMCP", "MOTD", "ANSI", "CREDITS", "MCCP")
        help.motd.get.text.length() should be > 100
        help.greeting.get.text.length() should be > 100
    }

    "The Crypt object" should "encryptAsUtf8" in {
        Crypt.hashAsUtf8("plaintext").length() should be >= 10
    }
}
