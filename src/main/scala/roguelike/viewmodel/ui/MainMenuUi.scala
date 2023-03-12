package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap
import roguelike.components.ui.ButtonComponent

final case class MainMenuUi(
    newGame: ButtonComponent,
    loadGame: Option[ButtonComponent],
    position: Point
):
  val scale: Int = newGame.scale

  def view(context: SceneContext[Size]): Batch[SceneNode] =
    loadGame match {
      case Some(b) => newGame.draw ++ b.draw
      case None    => newGame.draw
    }

  def moveTo(pos: Point): MainMenuUi =
    copy(
      newGame = newGame.moveTo(pos),
      loadGame = loadGame.map(l => l.moveTo(pos.withY(pos.y + (50 * scale))))
    )

  def withScale(scale: Int): MainMenuUi =
    copy(
      newGame = newGame.withScale(scale),
      loadGame = loadGame.map(l => l.withScale(scale))
    ).moveTo(position)

object MainMenuUi:
  def apply(
      newGame: Batch[GlobalEvent],
      loadGame: Option[Batch[GlobalEvent]]
  ): MainMenuUi =
    val buttonSize = 208
    MainMenuUi(
      ButtonComponent("[n] Play a new game", buttonSize, newGame),
      loadGame.map(l => ButtonComponent("[c] Continue last game", buttonSize, l)),
      Point.zero
    )

  def apply(
      newGame: Batch[GlobalEvent]
  ): MainMenuUi = MainMenuUi(newGame, None)
