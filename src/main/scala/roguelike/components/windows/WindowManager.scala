package roguelike.components.windows

import indigo.*
import indigo.scenes.SceneContext
import roguelike.components.Component
import roguelike.model.GameState
import roguelike.model.Model
import roguelike.viewmodel.GameViewModel

object WindowManager extends Component[Size, Model, GameViewModel]:
  type Command            = WindowManagerCommand
  type ComponentModel     = Model
  type ComponentViewModel = GameViewModel

  def modelLens: Lens[Model, Model] =
    Lens.keepLatest

  def viewModelLens: Lens[GameViewModel, GameViewModel] =
    Lens.keepLatest

  val updateActive: Lens[Model, ActiveWindow] =
    Lens(
      _.windowManager,
      (m, next) => m.copy(windowManager = next)
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

    case WindowManagerCommand.CloseAll =>
      Outcome(updateActive.set(model.closeAllWindows, ActiveWindow.None))

    case WindowManagerCommand.HandleQuitKeyPress =>
      model.windowManager match
        case ActiveWindow.Quit =>
          Outcome(updateActive.set(model.closeAllWindows, ActiveWindow.None))

        case ActiveWindow.None =>
          Outcome(updateActive.set(model.pauseForWindow, ActiveWindow.Quit))

        case _ =>
          Outcome(model)

    case WindowManagerCommand.DelegateInput(key) =>
      model.windowManager match
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
  ): Batch[SceneNode] =
    model.windowManager match
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
        History.present(context, model, viewModel)

      case ActiveWindow.None =>
        Batch.empty

  val initialModel: ActiveWindow =
    ActiveWindow.None

  def showingWindow(model: Model): Boolean =
    modelLens.get(model).windowManager match
      case ActiveWindow.None => false
      case _                 => true

  def showingUnCloseableWindow(model: Model): Boolean =
    showingWindow(model) && isUnCloseable(model)

  def showingCloseableWindow(model: Model): Boolean =
    showingWindow(model) && isCloseable(model)

  def isCloseable(model: Model): Boolean =
    modelLens.get(model).windowManager.closeable

  def isUnCloseable(model: Model): Boolean =
    !modelLens.get(model).windowManager.closeable

enum WindowManagerCommand:
  case ShowQuit
  case ShowLevelUp
  case ShowDropMenu
  case ShowEquipMenu
  case ShowInventoryMenu
  case ShowHistory
  case CloseAll
  case HandleQuitKeyPress
  case DelegateInput(key: Key)

enum ActiveWindow(val closeable: Boolean):
  case None          extends ActiveWindow(true)
  case Quit          extends ActiveWindow(true)
  case LevelUp       extends ActiveWindow(false)
  case DropMenu      extends ActiveWindow(true)
  case EquipMenu     extends ActiveWindow(true)
  case History       extends ActiveWindow(true)
  case InventoryMenu extends ActiveWindow(true)
