package roguelike.components.windows

import indigo.*
import indigo.scenes.Lens
import roguelike.components.Component
import roguelike.model.GameState
import roguelike.model.Model
import roguelike.viewmodel.GameViewModel

object WindowManager extends Component[Model, GameViewModel]:
  type Command            = WindowManagerCommand
  type ComponentModel     = Model
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Model] =
    Lens.keepLatest

  def viewModelLens: Lens[GameViewModel, Size] =
    Lens.readOnly(_.viewportSize)

  val updateActive: Lens[Model, ActiveWindow] =
    Lens(
      _.windowManager,
      (m, next) => m.copy(windowManager = next)
    )

  def nextModel(
      model: Model
  ): WindowManagerCommand => Outcome[Model] =
    case WindowManagerCommand.ShowQuit =>
      Outcome(updateActive.set(model, ActiveWindow.Quit))

    case WindowManagerCommand.CloseAll =>
      Outcome(updateActive.set(model, ActiveWindow.None))

    case WindowManagerCommand.HandleQuitKeyPress =>
      model.windowManager match
        case ActiveWindow.Quit =>
          Outcome(updateActive.set(model, ActiveWindow.None))

        case ActiveWindow.None =>
          Outcome(updateActive.set(model, ActiveWindow.Quit))

    case WindowManagerCommand.DelegateInput(key) =>
      model.windowManager match
        case ActiveWindow.Quit =>
          QuitMenu.updateModel(model, QuitMenu.HandleInput(key))

        case ActiveWindow.None =>
          Outcome(model)

  def nextViewModel(
      model: Model,
      viewModel: Size
  ): WindowManagerCommand => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      model: Model,
      viewModel: Size
  ): Batch[SceneNode] =
    model.windowManager match
      case ActiveWindow.Quit =>
        QuitMenu.present(model, viewModel)

      case ActiveWindow.None =>
        Batch.empty

  val initialModel: ActiveWindow =
    ActiveWindow.None

  def showingWindow(model: Model): Boolean =
    modelLens.get(model).windowManager match
      case ActiveWindow.None => false
      case _                 => true

enum WindowManagerCommand:
  case ShowQuit
  case CloseAll
  case HandleQuitKeyPress
  case DelegateInput(key: Key)

enum ActiveWindow:
  case None
  case Quit
