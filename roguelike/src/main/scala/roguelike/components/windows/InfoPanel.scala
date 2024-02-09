package roguelike.components.windows

import indigo.*
import roguelike.ColorScheme
import roguelike.model.GameWindowContext
import roguelike.model.entity.Player
import roguelikestarterkit.*

object InfoPanel:

  private val graphic = Graphic(0, 0, TerminalMaterial(AssetName(""), RGBA.White, RGBA.Black))

  def window(
      charSheet: CharSheet
  ): WindowModel[Size, GameWindowContext, Unit] =
    WindowModel(
      WindowId("Info"),
      charSheet,
      ()
    )
      .withTitle("Info")
      .moveTo(0, 3)
      .resizeTo(
        Dimensions(25, 7)
      )
      .isDraggable
      .updateModel(updateModel)
      .present(present)

  def updateModel(
      context: UiContext[Size, GameWindowContext],
      model: Unit
  ): GlobalEvent => Outcome[Unit] =
    case _ =>
      Outcome(model)

  def present(
      context: UiContext[Size, GameWindowContext],
      model: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        renderLevel(
          context.charSheet,
          context.screenSpaceBounds.position,
          context.data.currentFloor
        ),
        renderBar(
          context.charSheet,
          context.data.player,
          20,
          context.screenSpaceBounds.position + Point(0, context.charSheet.size.height + 4)
        )
      )
    )

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
