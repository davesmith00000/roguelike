package roguelike.components

import indigo.*
import indigo.scenes.Lens

trait Component[ParentModel, ParentViewModel]:
  type ComponentModel
  type ComponentViewModel
  type Command

  def modelLens: Lens[ParentModel, ComponentModel]
  def viewModelLens: Lens[ParentViewModel, ComponentViewModel]

  def updateModel(model: ParentModel, cmd: Command): Outcome[ParentModel] =
    val n = nextModel(modelLens.get(model))(cmd)
    n.map(nn => modelLens.set(model, nn))

  def nextModel(
      model: ComponentModel
  ): Command => Outcome[ComponentModel]

  def updateViewModel(
      model: ParentModel,
      viewModel: ParentViewModel,
      cmd: Command
  ): Outcome[ParentViewModel] =
    val n = nextViewModel(
      modelLens.get(model),
      viewModelLens.get(viewModel)
    )(cmd)
    n.map(nn => viewModelLens.set(viewModel, nn))

  def nextViewModel(
      model: ComponentModel,
      viewModel: ComponentViewModel
  ): Command => Outcome[ComponentViewModel]

  def present(
      model: ParentModel,
      viewModel: ParentViewModel
  ): Batch[SceneNode] =
    view(modelLens.get(model), viewModelLens.get(viewModel))

  def view(
      model: ComponentModel,
      viewModel: ComponentViewModel
  ): Batch[SceneNode]
