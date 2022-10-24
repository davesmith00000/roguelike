package roguelike.viewmodel.ui

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap

trait UiViewModel:
  def view(context: SceneContext[Size]): Batch[SceneNode]
