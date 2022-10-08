package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext

final case class GameUi() extends UiViewModel:
  def view(context: SceneContext[Size]): Batch[SceneNode] =
    Batch.empty
