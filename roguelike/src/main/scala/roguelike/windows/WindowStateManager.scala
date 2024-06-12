package roguelike.windows

import indigo.*
import indigo.scenes.SceneContext
import roguelike.GameEvent
import roguelike.model.Model
import roguelikestarterkit.WindowEvent

object WindowStateManager:

  val updateActive: Lens[Model, ActiveWindow] =
    Lens(
      _.activeWindow,
      (m, next) => m.copy(activeWindow = next)
    )

  def updateModel(
      context: SceneContext[Size],
      model: Model
  ): WindowManagerCommand => Outcome[Model] =
    case WindowManagerCommand.ShowQuit =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.Quit))
        .addGlobalEvents(WindowEvent.Open(QuitSaveWindow.windowId))

    case WindowManagerCommand.ShowLevelUp =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.LevelUp))
        .addGlobalEvents(WindowEvent.Open(LevelUpWindow.windowId))

    case WindowManagerCommand.ShowDropMenu =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.DropMenu))
        .addGlobalEvents(WindowEvent.Open(DropWindow.windowId))

    case WindowManagerCommand.ShowEquipMenu =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.EquipMenu))

    case WindowManagerCommand.ShowInventoryMenu =>
      Outcome(
        updateActive.set(model.pauseForWindow, ActiveWindow.InventoryMenu)
      )

    case WindowManagerCommand.ShowHistory =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.History))
        .addGlobalEvents(WindowEvent.Open(HistoryWindow.windowId), GameEvent.RedrawHistoryLog)

    case WindowManagerCommand.ShowControls =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.Controls))
        .addGlobalEvents(WindowEvent.Open(ControlsWindow.windowId))

    case WindowManagerCommand.CloseAll =>
      val closeActive: Batch[WindowEvent] =
        model.activeWindow match
          case ActiveWindow.None          => Batch.empty
          case ActiveWindow.Quit          => Batch(WindowEvent.Close(QuitSaveWindow.windowId))
          case ActiveWindow.LevelUp       => Batch(WindowEvent.Close(LevelUpWindow.windowId))
          case ActiveWindow.DropMenu      => Batch(WindowEvent.Close(DropWindow.windowId))
          case ActiveWindow.EquipMenu     => Batch()
          case ActiveWindow.History       => Batch(WindowEvent.Close(HistoryWindow.windowId))
          case ActiveWindow.InventoryMenu => Batch()
          case ActiveWindow.Controls      => Batch(WindowEvent.Close(ControlsWindow.windowId))

      Outcome(updateActive.set(model.closeAllWindows, ActiveWindow.None))
        .addGlobalEvents(closeActive)

  val initialModel: ActiveWindow =
    ActiveWindow.None

  def showingWindow(model: Model): Boolean =
    model.activeWindow match
      case ActiveWindow.None => false
      case _                 => true

  def showingUnCloseableWindow(model: Model): Boolean =
    showingWindow(model) && isUnCloseable(model)

  def showingCloseableWindow(model: Model): Boolean =
    showingWindow(model) && isCloseable(model)

  def isCloseable(model: Model): Boolean =
    model.activeWindow.closeable

  def isUnCloseable(model: Model): Boolean =
    !model.activeWindow.closeable

enum ActiveWindow(val closeable: Boolean):
  case None          extends ActiveWindow(true)
  case Quit          extends ActiveWindow(true)
  case LevelUp       extends ActiveWindow(false)
  case DropMenu      extends ActiveWindow(true)
  case EquipMenu     extends ActiveWindow(true)
  case History       extends ActiveWindow(true)
  case InventoryMenu extends ActiveWindow(true)
  case Controls      extends ActiveWindow(true)
