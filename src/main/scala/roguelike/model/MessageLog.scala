package roguelike.model

import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.RGBA
import indigo.shared.datatypes.Size
import io.circe._
import io.circe.syntax._
import io.indigoengine.roguelike.starterkit.*
import roguelike.ColorScheme
import roguelike.RogueLikeGame
import roguelike.assets.Assets

final case class MessageLog(messages: List[Message], maxLength: Option[Int]):

  def logLength: Int =
    messages.length

  def addMessage(message: Message): MessageLog =
    if message.stackable then
      this.copy(
        messages = messages.headOption
          .map { m =>
            val msgs =
              if m.text == message.text then m.increaseCount :: messages.drop(1)
              else message :: m :: messages.drop(1)

            maxLength match
              case None      => msgs
              case Some(max) => msgs.take(max)
          }
          .getOrElse(List(message))
      )
    else this.copy(messages = message :: messages)

  def withMaxLength(newMax: Int): MessageLog =
    this.copy(maxLength = Option(newMax))
  def noLimit: MessageLog =
    this.copy(maxLength = None)

  def toTerminal(
      size: Size,
      reversed: Boolean,
      startOffset: Int,
      fadeOut: Boolean
  ): TerminalEmulator =
    MessageLog.logToTerminal(size, messages, reversed, startOffset, fadeOut)

  def render(
      position: Point,
      size: Size,
      reversed: Boolean,
      startOffset: Int
  ): TerminalEntity =
    MessageLog.renderMessages(
      position * RogueLikeGame.charSize.toPoint,
      size * RogueLikeGame.charSize,
      messages,
      reversed,
      startOffset
    )

object MessageLog:

  given Encoder[MessageLog] = new Encoder[MessageLog] {
    final def apply(data: MessageLog): Json = Json.obj(
      ("messages", data.messages.asJson),
      ("maxLength", data.maxLength.asJson)
    )
  }

  given Decoder[MessageLog] = new Decoder[MessageLog] {
    final def apply(c: HCursor): Decoder.Result[MessageLog] =
      for {
        messages  <- c.downField("messages").as[List[Message]]
        maxLength <- c.downField("maxLength").as[Option[Int]]
      } yield MessageLog(messages, maxLength)
  }

  def Unlimited: MessageLog =
    MessageLog(Nil, None)

  def Limited(maxLength: Int): MessageLog =
    MessageLog(Nil, Option(maxLength))

  def DefaultLimited: MessageLog =
    MessageLog(Nil, Option(30))

  def logToTerminal(
      size: Size,
      messages: List[Message],
      reversed: Boolean,
      startOffset: Int,
      fadeOut: Boolean
  ): TerminalEmulator =
    val msgs =
      (if reversed then messages.reverse else messages).drop(startOffset)
    msgs
      .take(size.height)
      .foldLeft((TerminalEmulator(size), 0)) { case ((t, r), m) =>
        val darkenAmount =
          if fadeOut then (0.8 * (r.toDouble / size.height.toDouble)) + 0.2
          else 0.0
        (
          t.putLine(
            Point(0, r),
            m.fullText,
            m.fgColor.mix(RGB.Black, darkenAmount),
            RGBA.Black
          ),
          r + 1
        )
      }
      ._1

  def renderMessages(
      position: Point,
      size: Size,
      messages: List[Message],
      reversed: Boolean,
      startOffset: Int
  ): TerminalEntity =
    logToTerminal(size, messages, reversed, startOffset, true)
      .draw(
        Assets.Basic.tileMap,
        RogueLikeGame.charSize,
        MapTile(Tile.SPACE),
        4000
      )
      .moveTo(position)
