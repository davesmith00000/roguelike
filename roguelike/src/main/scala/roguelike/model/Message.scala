package roguelike.model

import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGBA
import indigo.shared.datatypes.Size
import io.circe.*
import io.circe.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.ColorScheme
import roguelike.RogueLikeGame
import roguelike.assets.GameAssets

final case class Message(
    text: String,
    fgColor: RGBA,
    count: Int,
    stackable: Boolean
):
  def fullText: String =
    if count > 1 then s"$text (x$count)"
    else text

  def withText(newText: String): Message =
    this.copy(text = newText)

  def withFgColor(newColor: RGBA): Message =
    this.copy(fgColor = newColor)

  def withCount(newCount: Int): Message =
    this.copy(count = newCount)
  def increaseCount: Message =
    withCount(count + 1)
  def decreaseCount: Message =
    withCount(count - 1)

  def makeStackable: Message =
    this.copy(stackable = true)

  def unStackable: Message =
    this.copy(stackable = false)

object Message:

  import SharedCodecs.given

  given Encoder[Message] = new Encoder[Message] {
    final def apply(data: Message): Json = Json.obj(
      ("text", Json.fromString(data.text)),
      ("fgColor", data.fgColor.asJson),
      ("count", Json.fromInt(data.count)),
      ("stackable", Json.fromBoolean(data.stackable))
    )
  }

  given Decoder[Message] = new Decoder[Message] {
    final def apply(c: HCursor): Decoder.Result[Message] =
      for {
        text      <- c.downField("text").as[String]
        fgColor   <- c.downField("fgColor").as[RGBA]
        count     <- c.downField("count").as[Int]
        stackable <- c.downField("stackable").as[Boolean]
      } yield Message(text, fgColor, count, stackable)
  }

  def apply(text: String, fgColor: RGBA): Message =
    Message(text, fgColor, 1, true)

  val thatWayIsBlocked: Message =
    Message("That way is blocked.", ColorScheme.impossible)

  val thereIsNothingHereToPickUp: Message =
    Message("There is nothing here to pick up.", ColorScheme.impossible)
