package mud

import mud._

import java.nio.charset._
import java.nio.file._
import java.io._

import argonaut._, Argonaut._

object Save
{
    implicit def PersonCodecJson: CodecJson[dMobile] =
        CodecJson(
            (m: dMobile) =>
                ("name" := m.name) ->:
                ("password" := m.password) ->:
                ("level" := m.level) ->:
                jEmptyObject,
            c => for {
                name <- (c --\ "name").as[String]
                password <- (c --\ "password").as[String]
                level <- (c --\ "level").as[Int]
            } yield dMobile(None, Nil, name, password, level))


    def save_player(dMob: dMobile): Unit = {
        val br = Files.newBufferedWriter(Paths.get("./data/players/" + dMob.name), Charset.forName("US-ASCII"))
        try {
            br.write(dMob.asJson.spaces4)
        } finally {
            br.close()
        }
    }

    def load_player(playerName: String): Option[dMobile] = {
        val s = new String(Files.readAllBytes(Paths.get("./data/players/" + playerName)), StandardCharsets.UTF_8)
        s.decodeOption[dMobile]
    }

    def load_profile = load_player(_)

}
