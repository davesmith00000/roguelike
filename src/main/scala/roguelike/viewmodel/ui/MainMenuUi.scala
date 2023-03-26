package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap
import roguelike.components.ui.ButtonComponent

final case class MainMenuUi(
    newGame: ButtonComponent,
    loadGame: ButtonComponent,
    position: Point,
    canLoadGame: Boolean
):
  val scale: Int = newGame.scale

  def view: Batch[SceneNode] =
    val activeShadowColor = RGB.Orange.mix(RGB.Black, 0.25)
    newGame.draw(RGB.White, activeShadowColor) ++
      (if canLoadGame then loadGame.draw(RGB.White, activeShadowColor) else loadGame.draw(RGB.SlateGray, RGB.Black))

  def moveTo(pos: Point): MainMenuUi =
    copy(
      newGame = newGame.moveTo(pos),
      loadGame = loadGame.moveTo(pos.withY(pos.y + (25 * scale)))
    )

  def withScale(scale: Int): MainMenuUi =
    copy(
      newGame = newGame.withScale(scale),
      loadGame = loadGame.withScale(scale)
    ).moveTo(position)

object MainMenuUi:
  def apply(
      newGame: Batch[GlobalEvent],
      loadGame: Batch[GlobalEvent],
      canLoadGame: Boolean
  ): MainMenuUi =
    val buttonSize = 208
    MainMenuUi(
      ButtonComponent("[n] Play a new game", buttonSize, newGame),
      ButtonComponent("[c] Continue last game", buttonSize, loadGame),
      Point.zero,
      canLoadGame
    )
