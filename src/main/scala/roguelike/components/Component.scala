package roguelike.components

import indigo.*
import indigo.scenes.Lens

trait Component[StartupData, ParentModel, ParentViewModel]:
  type ComponentModel
  type ComponentViewModel
  type Command

  def modelLens: Lens[ParentModel, ComponentModel]
  def viewModelLens: Lens[ParentViewModel, ComponentViewModel]

  def updateModel(
      context: FrameContext[StartupData],
      model: ParentModel,
      cmd: Command
  ): Outcome[ParentModel] =
    val n = nextModel(context, modelLens.get(model))(cmd)
    n.map(nn => modelLens.set(model, nn))

  def nextModel(
      context: FrameContext[StartupData],
      model: ComponentModel
  ): Command => Outcome[ComponentModel]

  def updateViewModel(
      context: FrameContext[StartupData],
      model: ParentModel,
      viewModel: ParentViewModel,
      cmd: Command
  ): Outcome[ParentViewModel] =
    val n = nextViewModel(
      context,
      modelLens.get(model),
      viewModelLens.get(viewModel)
    )(cmd)
    n.map(nn => viewModelLens.set(viewModel, nn))

  def nextViewModel(
      context: FrameContext[StartupData],
      model: ComponentModel,
      viewModel: ComponentViewModel
  ): Command => Outcome[ComponentViewModel]

  def present(
      context: FrameContext[StartupData],
      model: ParentModel,
      viewModel: ParentViewModel
  ): Batch[SceneNode] =
    view(context, modelLens.get(model), viewModelLens.get(viewModel))

  def view(
      context: FrameContext[StartupData],
      model: ComponentModel,
      viewModel: ComponentViewModel
  ): Batch[SceneNode]
