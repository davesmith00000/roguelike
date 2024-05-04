package roguelike.components.windows

import indigo._
import indigo.scenes.SceneContext
import roguelike.GameEvent
import roguelike.components.GameComponent
import roguelike.model.Model
import roguelike.viewmodel.GameViewModel
import roguelike.windows.ControlsWindow
import roguelike.windows.HistoryWindow
import roguelikestarterkit.WindowEvent

object WindowStateManager extends GameComponent[Size, Model, GameViewModel]:
  type Command            = WindowManagerCommand
  type ComponentModel     = Model
  type ComponentViewModel = GameViewModel

  def modelLens: Lens[Model, Model] =
    Lens.keepLatest

  def viewModelLens: Lens[GameViewModel, GameViewModel] =
    Lens.keepLatest

  val updateActive: Lens[Model, ActiveWindow] =
    Lens(
      _.activeWindow,
      (m, next) => m.copy(activeWindow = next)
    )

  def nextModel(
      context: SceneContext[Size],
      model: Model
  ): WindowManagerCommand => Outcome[Model] =
    case WindowManagerCommand.ShowQuit =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.Quit))

    case WindowManagerCommand.ShowLevelUp =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.LevelUp))

    case WindowManagerCommand.ShowDropMenu =>
      Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.DropMenu))

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
          case ActiveWindow.Quit          => Batch()
          case ActiveWindow.LevelUp       => Batch()
          case ActiveWindow.DropMenu      => Batch()
          case ActiveWindow.EquipMenu     => Batch()
          case ActiveWindow.History       => Batch(WindowEvent.Close(HistoryWindow.windowId))
          case ActiveWindow.InventoryMenu => Batch()
          case ActiveWindow.Controls      => Batch(WindowEvent.Close(ControlsWindow.windowId))

      Outcome(updateActive.set(model.closeAllWindows, ActiveWindow.None))
        .addGlobalEvents(closeActive)

    case WindowManagerCommand.HandleQuitKeyPress =>
      model.activeWindow match
        case ActiveWindow.Quit =>
          Outcome(updateActive.set(model.closeAllWindows, ActiveWindow.None))

        case ActiveWindow.None =>
          Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.Quit))

        case _ =>
          Outcome(model)

    case WindowManagerCommand.DelegateInput(key) =>
      model.activeWindow match
        case ActiveWindow.Quit =>
          QuitMenu.updateModel(context, model, QuitMenu.HandleInput(key))

        case ActiveWindow.LevelUp =>
          LevelUp.updateModel(context, model, LevelUp.HandleInput(key))

        case ActiveWindow.DropMenu =>
          DropMenu.updateModel(context, model, DropMenu.HandleInput(key))

        case ActiveWindow.EquipMenu =>
          EquipMenu.updateModel(context, model, EquipMenu.HandleInput(key))

        case ActiveWindow.InventoryMenu =>
          InventoryMenu.updateModel(
            context,
            model,
            InventoryMenu.HandleInput(key)
          )

        case ActiveWindow.History =>
          Outcome(model)

        case ActiveWindow.Controls =>
          Outcome(model)

        case ActiveWindow.None =>
          Outcome(model)

  def nextViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): WindowManagerCommand => Outcome[GameViewModel] =
    _ => Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[Batch[SceneNode]] =
    model.activeWindow match
      case ActiveWindow.Quit =>
        QuitMenu.present(context, model, viewModel)

      case ActiveWindow.LevelUp =>
        LevelUp.present(context, model, viewModel)

      case ActiveWindow.DropMenu =>
        DropMenu.present(context, model, viewModel)

      case ActiveWindow.EquipMenu =>
        EquipMenu.present(context, model, viewModel)

      case ActiveWindow.InventoryMenu =>
        InventoryMenu.present(context, model, viewModel)

      case ActiveWindow.History =>
        Outcome(Batch.empty)

      case ActiveWindow.Controls =>
        Outcome(Batch.empty)

      case ActiveWindow.None =>
        Outcome(Batch.empty)

  val initialModel: ActiveWindow =
    ActiveWindow.None

  def showingWindow(model: Model): Boolean =
    modelLens.get(model).activeWindow match
      case ActiveWindow.None => false
      case _                 => true

  def showingUnCloseableWindow(model: Model): Boolean =
    showingWindow(model) && isUnCloseable(model)

  def showingCloseableWindow(model: Model): Boolean =
    showingWindow(model) && isCloseable(model)

  def isCloseable(model: Model): Boolean =
    modelLens.get(model).activeWindow.closeable

  def isUnCloseable(model: Model): Boolean =
    !modelLens.get(model).activeWindow.closeable

enum ActiveWindow(val closeable: Boolean):
  case None          extends ActiveWindow(true)
  case Quit          extends ActiveWindow(true)
  case LevelUp       extends ActiveWindow(false)
  case DropMenu      extends ActiveWindow(true)
  case EquipMenu     extends ActiveWindow(true)
  case History       extends ActiveWindow(true)
  case InventoryMenu extends ActiveWindow(true)
  case Controls      extends ActiveWindow(true)
