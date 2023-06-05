package roguelike.components.ui

import indigo.*
import indigo.shared.scenegraph.*
import indigoextras.ui.HitArea
import io.indigoengine.roguelike.starterkit.terminal.TerminalText
import io.indigoengine.roguelike.starterkit.tiles.RoguelikeTiles
import roguelike.assets.GameAssets

final case class ButtonComponent(
    text: String,
    width: Int,
    position: Point,
    hitArea: HitArea,
    scale: Int
):
  private val tileWidth =
    (ButtonComponent.baseTile * ButtonComponent.multiplier).toInt
  val height = (ButtonComponent.baseTile * ButtonComponent.multiplier).toInt

  def moveTo(pos: Point): ButtonComponent =
    copy(position = pos, hitArea = hitArea.moveTo(pos))

  def moveTo(x: Int, y: Int): ButtonComponent =
    moveTo(Point(x, y))

  def moveBy(amount: Point): ButtonComponent =
    copy(
      position = position.moveBy(amount),
      hitArea = hitArea.moveBy(amount)
    )

  def moveBy(x: Int, y: Int): ButtonComponent =
    moveBy(Point(x, y))

  def withScale(scale: Int): ButtonComponent =
    copy(
      hitArea = hitArea.copy(area = hitArea.area.scaleBy(Vertex(scale.toDouble))),
      scale = scale
    )

  def update(mouse: Mouse) =
    hitArea.update(mouse).map(ha => this.copy(hitArea = ha))

  def draw(textColor: RGBA, shadowColor: RGBA): Batch[Group] =
    Batch(
      Group(
        Text(
          text,
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(GameAssets.TileMap, textColor, RGBA.Zero, shadowColor)
        ).alignCenter
          .moveTo(Point((width * 0.5).toInt, 4))
      ).moveTo(position)
        .scaleBy(Vector2(scale))
    )

object ButtonComponent:
  private val baseTile   = 16
  private val multiplier = 1.25

  def apply(
      text: String,
      width: Int,
      position: Point,
      actions: => Batch[GlobalEvent]
  ): ButtonComponent =
    ButtonComponent(
      text,
      width,
      position,
      HitArea(Rectangle(position, Size(width, (baseTile * multiplier).toInt)))
        .withClickActions(actions),
      1
    )

  def apply(
      text: String,
      width: Int,
      actions: => Batch[GlobalEvent]
  ): ButtonComponent =
    ButtonComponent(
      text,
      width,
      Point.zero,
      actions
    )
