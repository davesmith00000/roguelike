package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext
import roguelike.components.ui.ButtonComponent

final case class MainMenuUi(
    newGame: ButtonComponent,
    loadGame: Option[ButtonComponent]
) extends UiViewModel:
  def view(context: SceneContext[Size]): Batch[SceneNode] =
    loadGame match {
      case Some(b) => Batch(newGame.draw(), b.draw())
      case None    => Batch(newGame.draw())
    }

object MainMenuUi:
  def apply(
      newGame: Batch[GlobalEvent],
      loadGame: Option[Batch[GlobalEvent]]
  ): MainMenuUi =
    val buttonSize = 208
    MainMenuUi(
      ButtonComponent("[n] Play a new game", buttonSize, newGame),
      loadGame.map(l =>
        ButtonComponent("[c] Continue last game", buttonSize, l).moveTo(0, 50)
      )
    )

  def apply(
      newGame: Batch[GlobalEvent]
  ): MainMenuUi = MainMenuUi(newGame, None)
