package roguelike.components.windows

import indigo.*
import indigo.shared.scenegraph.CloneTiles
import io.indigoengine.roguelike.starterkit.*
import roguelike.components.Component
import roguelike.model.Model
import roguelike.viewmodel.GameViewModel

object History extends Component[Size, Model, GameViewModel]:
  type Command            = Unit
  type ComponentModel     = Model
  type ComponentViewModel = HistoryViewModel

  def modelLens: Lens[Model, Model] = Lens.identity
  def viewModelLens: Lens[GameViewModel, HistoryViewModel] =
    Lens.readOnly { vm =>
      HistoryViewModel(
        vm.viewportSize,
        vm.terminals.history.clones
      )
    }

  def nextModel(
      context: FrameContext[Size],
      model: Model
  ): Unit => Outcome[Model] =
    _ => Outcome(model)

  def nextViewModel(
      context: FrameContext[Size],
      model: Model,
      viewModel: HistoryViewModel
  ): Unit => Outcome[HistoryViewModel] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      model: Model,
      viewModel: HistoryViewModel
  ): Batch[SceneNode] =
    val windowSize = Size(500, 310)

    Batch(
      Group(
        Shape.Box(
          Rectangle(Point.zero, windowSize),
          Fill.Color(RGBA.Black),
          Stroke(2, RGBA.Orange)
        ) :: viewModel.historyClones
      ).moveTo(((viewModel.viewportSize - windowSize) / 2).toPoint)
    )

  final case class HistoryViewModel(
      viewportSize: Size,
      historyClones: Batch[CloneTiles]
  )
