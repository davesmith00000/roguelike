package roguelike.model

import indigo.shared.IndigoLogger
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.Size
import io.circe._
import io.circe.parser.decode
import io.circe.syntax._
import roguelike.model.entity._

final case class ModelSaveData(
    screenSize: Size,
    player: Player,
    stairsPosition: Point,
    gameMap: GameMap,
    messageLog: MessageLog,
    currentFloor: Int
):
  def toJsonString: String =
    this.asJson.noSpaces

object ModelSaveData:

  def fromJsonString(json: String): Option[ModelSaveData] =
    decode[ModelSaveData](json) match
      case Left(e) =>
        IndigoLogger.debug(e.toString)
        None

      case Right(d) =>
        Option(d)

  val saveKey: String = "indigo_roguelike"

  import SharedCodecs.given

  given Encoder[ModelSaveData] = new Encoder[ModelSaveData] {
    final def apply(data: ModelSaveData): Json = Json.obj(
      ("screenSize", data.screenSize.asJson),
      ("player", data.player.asJson),
      ("stairsPosition", data.stairsPosition.asJson),
      ("gameMap", data.gameMap.asJson),
      ("messageLog", data.messageLog.asJson),
      ("currentFloor", data.currentFloor.asJson)
    )
  }

  given Decoder[ModelSaveData] = new Decoder[ModelSaveData] {
    final def apply(c: HCursor): Decoder.Result[ModelSaveData] =
      for {
        screenSize     <- c.downField("screenSize").as[Size]
        player         <- c.downField("player").as[Player]
        stairsPosition <- c.downField("stairsPosition").as[Point]
        gameMap        <- c.downField("gameMap").as[GameMap]
        messageLog     <- c.downField("messageLog").as[MessageLog]
        currentFloor   <- c.downField("currentFloor").as[Int]
      } yield ModelSaveData(
        screenSize,
        player,
        stairsPosition,
        gameMap,
        messageLog,
        currentFloor
      )
  }
