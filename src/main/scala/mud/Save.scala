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
        val playerPath = Paths.get("./data/players/" + dMob.name)
        Files.createDirectories(playerPath.getParent());
        val br = Files.newBufferedWriter(playerPath, Charset.forName("US-ASCII"))
        try {
            br.write(dMob.asJson.spaces4)
        } finally {
            br.close()
        }
    }

    def load_player(playerName: String): Option[dMobile] = {
        val playerPath = Paths.get("./data/players/" + playerName)
        if (Files.exists(playerPath)) {
            val s = new String(Files.readAllBytes(playerPath), StandardCharsets.UTF_8)
            s.decodeOption[dMobile]
        } else {
            None
        }
    }

    def load_profile = load_player(_)
}
