package roguelike.components.ui

import indigo.*
import indigo.shared.scenegraph.*
import indigoextras.geometry.Vertex
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
      hitArea =
        hitArea.copy(area = hitArea.area.scaleBy(Vertex(scale.toDouble))),
      scale = scale
    )

  def update(mouse: Mouse) =
    hitArea.update(mouse).map(ha => this.copy(hitArea = ha))

  def draw: Batch[Group] =
    Batch(
      Group(
        Graphic(16, 16, Material.Bitmap(GameAssets.UI))
          .withCrop(108, 308, 16, 16)
          .withScale(Vector2(ButtonComponent.multiplier)),
        Graphic(
          width - (tileWidth * 2),
          16,
          Material.Bitmap(GameAssets.UI).tile
        )
          .withCrop(126, 308, 16, 16)
          .moveTo(Point(tileWidth, 0))
          .withScale(Vector2(ButtonComponent.multiplier)),
        Graphic(16, 16, Material.Bitmap(GameAssets.UI))
          .withCrop(144, 308, 16, 16)
          .moveTo(Point(width - tileWidth, 0))
          .withScale(Vector2(ButtonComponent.multiplier)),
        Text(
          text,
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(GameAssets.TileMap, RGB.White, RGBA.Zero)
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
