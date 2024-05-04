package roguelike.windows

import indigo._
import roguelike.ColorScheme
import roguelike.model.GameWindowContext
import roguelike.model.entity.Player
import roguelikestarterkit._

object InfoPanel:

  val windowId: WindowId = WindowId("Info")

  def window(
      charSheet: CharSheet
  ): WindowModel[Unit, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      ()
    )
      .withTitle("Info")
      .moveTo(0, 3)
      .resizeTo(
        Dimensions(25, 7)
      )
      .isDraggable

  given WindowContent[Unit, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: Unit
    ): GlobalEvent => Outcome[Unit] =
      case _ =>
        Outcome(model)

    def present(
        context: UiContext[GameWindowContext],
        model: Unit
    ): Outcome[Layer] =
      Outcome(
        Layer(
          renderLevel(
            context.charSheet,
            context.screenSpaceBounds.position,
            context.reference.currentFloor
          ),
          renderBar(
            context.charSheet,
            context.reference.player,
            20,
            context.screenSpaceBounds.position + Point(0, context.charSheet.size.height + 4)
          )
        )
      )

    def cascade(model: Unit, newBounds: Bounds): Unit =
      model

  def renderBar(charSheet: CharSheet, player: Player, totalWidth: Int, position: Point): Group =
    val height = charSheet.size.height + 4
    val width  = charSheet.size.width * totalWidth
    val barWidth =
      (player.fighter.hp.toFloat / player.fighter.maxHp.toFloat * width).toInt

    Group(
      Shape.Box(
        Rectangle(0, 0, width, height),
        Fill.Color(ColorScheme.barEmpty)
      ),
      Shape.Box(
        Rectangle(0, 0, barWidth, height),
        Fill.Color(ColorScheme.barFilled)
      ),
      healthStatusLine(charSheet).withText(formatStatus(player))
    ).moveTo(position)

  def renderLevel(charSheet: CharSheet, position: Point, currentFloor: Int): Text[TerminalText] =
    healthStatusLine(charSheet)
      .withText("Dungeon level: " + currentFloor.toString)
      .moveTo(position)

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  def healthStatusLine(charSheet: CharSheet): Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(charSheet.assetName, RGBA.White, RGBA.Zero)
    )
      .moveTo(1, 2)
