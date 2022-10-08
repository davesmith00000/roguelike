package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext

trait UiViewModel:
  def view(context: SceneContext[Size]): Batch[SceneNode]
