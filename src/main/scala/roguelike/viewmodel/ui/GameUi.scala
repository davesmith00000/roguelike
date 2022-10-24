package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap

final case class GameUi() extends UiViewModel:
  def view(context: SceneContext[Size]): Batch[SceneNode] =
    Batch.empty
