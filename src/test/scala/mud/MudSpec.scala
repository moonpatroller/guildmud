package mud

import org.scalatest._

import java.nio.charset._
import java.nio.file._
import java.io._

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

    "The Ansi object" should "convertColorCodes" in {
        Ansi.convertColorCodes("") shouldBe ""
        Ansi.convertColorCodes("a") shouldBe "a"
        Ansi.convertColorCodes("d") shouldBe "d"
        Ansi.convertColorCodes("#") shouldBe "#"
        Ansi.convertColorCodes("##") shouldBe "#"
        assert(Ansi.convertColorCodes("#b" ).toList === "\u001b[34m\u001b[0m".toList)
        assert(Ansi.convertColorCodes("#ba").toList === "\u001b[34ma\u001b[0m".toList)
        assert(Ansi.convertColorCodes("#b ").toList === "\u001b[34m \u001b[0m".toList)
        assert(Ansi.convertColorCodes("#B" ).toList === "\u001b[1;34m\u001b[0m".toList)
        assert(Ansi.convertColorCodes("#Ba").toList === "\u001b[1;34ma\u001b[0m".toList)
        assert(Ansi.convertColorCodes("#B ").toList === "\u001b[1;34m \u001b[0m".toList)
    }

    "The Area object" should "serialize" in {
        val areaConfig = AreaConfig(List(TextProbability("first text", 0.5)), List(TextProbability("second text", 0.1)))
        val data = AreaConfig.serialize(areaConfig)
        val expected = """{
    "additional-descriptions" : [
        {
            "text" : "second text",
            "probability" : 0.1
        }
    ],
    "mutually-exclusive-descriptions" : [
        {
            "text" : "first text",
            "probability" : 0.5
        }
    ]
}""".replaceAll("\r", "")

        data shouldBe expected
        AreaConfig.deserialize(data) shouldBe Option(areaConfig)
    }
}
