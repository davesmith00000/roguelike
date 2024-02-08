package roguelike.model

import indigo.*
import io.circe.*
import io.circe.syntax.*
import roguelike.ColorScheme
import roguelike.RogueLikeGame
import roguelike.assets.GameAssets
import roguelikestarterkit.*
import roguelikestarterkit.*

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
  ): RogueTerminalEmulator =
    MessageLog.logToTerminal(size, messages, reversed, startOffset, fadeOut)

  def render(
      position: Point,
      size: Size,
      reversed: Boolean,
      startOffset: Int
  ): TerminalClones =
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
  ): RogueTerminalEmulator =
    val msgs =
      (if reversed then messages.reverse else messages).drop(startOffset)
    msgs
      .take(size.height)
      .foldLeft((RogueTerminalEmulator(size), 0)) { case ((t, r), m) =>
        val darkenAmount =
          if fadeOut then (0.8 * (r.toDouble / size.height.toDouble)) + 0.2
          else 0.0
        (
          t.putLine(
            Point(0, r),
            m.fullText,
            m.fgColor.mix(RGBA.Black, darkenAmount),
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
  ): TerminalClones =
    logToTerminal(size, messages, reversed, startOffset, true)
      .toCloneTiles(CloneId("messages"), position, RoguelikeTiles.Size10x10.charCrops) { (fg, bg) =>
        Graphic(10, 10, TerminalText(GameAssets.assets.init.AnikkiSquare10x10, fg, bg))
      }
