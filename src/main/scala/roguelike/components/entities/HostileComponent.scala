package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.components.Component
import roguelike.model.Model
import roguelike.model.entity.Hostile
import roguelike.viewmodel.GameViewModel

object HostileComponent extends Component[Size, Model, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = Hostile
  type ComponentViewModel = HostileVM

  def modelLens: Lens[Model, Hostile] =
    // Hmmm... there isn't enough information here to extract the right hostile from the ah. You pull out all of them? Need a hostiles mananger.
    ???

  def viewModelLens: Lens[GameViewModel, HostileVM] =
    Lens(
      viewModel => HostileVM(),
      (viewModel, hostileVM) =>
        viewModel
    )

  def nextModel(
      context: FrameContext[Size],
      model: Hostile
  ): Cmds => Outcome[Hostile] =
    _ => Outcome(model)

  def nextViewModel(
      context: FrameContext[Size],
      model: Hostile,
      viewModel: HostileVM
  ): Cmds => Outcome[HostileVM] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      model: Hostile,
      viewModel: HostileVM
  ): Batch[SceneNode] =
    Batch()

  enum Cmds:
    case Something

  final case class HostileVM()
