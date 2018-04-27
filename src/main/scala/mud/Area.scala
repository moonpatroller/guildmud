package mud

import mud._

import java.nio.charset._
import java.nio.file._
import java.io._

import argonaut._, Argonaut._

object AreaConfig
{
    implicit def TextProbabilityJson: CodecJson[TextProbability] =
        CodecJson(
            (textProbability: TextProbability) =>
                ("probability" := textProbability.probability) ->:
                ("text" := textProbability.text) ->:
                jEmptyObject,
            c => for {
                probability <- (c --\ "probability").as[Double]
                text <- (c --\ "text").as[String]
            } yield TextProbability(text, probability))

    implicit def AreaConfigJson: CodecJson[AreaConfig] =
        CodecJson(
            (areaConfig: AreaConfig) =>
                ("mutually-exclusive-descriptions" := areaConfig.mutuallyExclusiveDescriptions) ->:
                ("additional-descriptions" := areaConfig.additionalDescriptions) ->:
                jEmptyObject,
            c => for {
                mutuallyExclusiveDescriptions <- (c --\ "mutually-exclusive-descriptions").as[List[TextProbability]]
                additionalDescriptions <- (c --\ "additional-descriptions").as[List[TextProbability]]
            } yield AreaConfig(mutuallyExclusiveDescriptions, additionalDescriptions))

    def load(areaName: String): Option[AreaConfig] = {
        val path = Paths.get(".\\data\\areas\\" + areaName + ".json")
        println("path: " + path)
        if (Files.exists(path)) {
            val s = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
            println("path not bad: " + s)
            s.decodeOption[AreaConfig]
        } else {
            None
        }
    }

    def serialize(areaConfig: AreaConfig): String = {
        areaConfig.asJson.spaces4
    }

    def deserialize(input: String): Option[AreaConfig] = {
        input.decodeOption[AreaConfig]
    }
}

case class TextProbability(text: String, probability: Double)

case class AreaConfig(
    mutuallyExclusiveDescriptions: List[TextProbability],
    additionalDescriptions: List[TextProbability]
)

object Area
{
    def load(areaName: String): Option[Area] = {
        AreaConfig.load(areaName) map(new Area(_))
    }
}

class Area(config: AreaConfig)
{
    var mobiles = List[dMobile]()

    def add(mob: dMobile): Unit = {
        mobiles = mob :: mobiles
    }

    def doTick(): Unit = {
        mobiles foreach { mob => mob.socket.foreach { socket => socket.appendOutput(config + "\r\n") } }
    }
}
