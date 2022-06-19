package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.components.Component
import roguelike.model.HostilesPool
import roguelike.model.Model
import roguelike.model.entity.Hostile
import roguelike.viewmodel.GameViewModel

object HostilesManager extends Component[Size, Model, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = HostilesPool
  type ComponentViewModel = HostilesVM

  def modelLens: Lens[Model, HostilesPool] =
    Lens(
      _.hostiles,
      (m, hp) => m.copy(hostiles = hp)
    )

  def viewModelLens: Lens[GameViewModel, HostilesVM] =
    Lens(
      viewModel => HostilesVM(),
      (viewModel, hostileVM) => viewModel
    )

  def nextModel(
      context: FrameContext[Size],
      model: HostilesPool
  ): Cmds => Outcome[HostilesPool] =
    _ => Outcome(model)

  def nextViewModel(
      context: FrameContext[Size],
      model: HostilesPool,
      viewModel: HostilesVM
  ): Cmds => Outcome[HostilesVM] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      model: HostilesPool,
      viewModel: HostilesVM
  ): Batch[SceneNode] =
    Batch()

  enum Cmds:
    case Something

  final case class HostilesVM()
