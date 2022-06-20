package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.components.Component
import roguelike.model.HostilesPool
import roguelike.model.entity.Hostile
import roguelike.viewmodel.GameViewModel

object HostileComponent extends Component[Size, Hostile, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = Hostile
  type ComponentViewModel = HostileVM

  def modelLens: Lens[Hostile, Hostile] = Lens.keepLatest

  def viewModelLens: Lens[GameViewModel, HostileVM] =
    Lens(
      viewModel => HostileVM(),
      (viewModel, hostileVM) => viewModel
    )

  def nextModel(
      context: FrameContext[Size],
      hostile: Hostile
  ): Cmds => Outcome[Hostile] =
    case Cmds.ConfuseFor(numOfTurns) =>
      hostile.confuseFor(numOfTurns)

  def nextViewModel(
      context: FrameContext[Size],
      hostile: Hostile,
      viewModel: HostileVM
  ): Cmds => Outcome[HostileVM] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      hostile: Hostile,
      viewModel: HostileVM
  ): Batch[SceneNode] =
    Batch()

  enum Cmds:
    case ConfuseFor(numberOfTurns: Int)

  final case class HostileVM()
