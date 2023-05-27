package roguelike.components

import indigo.*
import indigo.scenes.SceneContext

trait Component[StartupData, ParentModel, ParentViewModel]:
  type ComponentModel
  type ComponentViewModel
  type Command

  def modelLens: Lens[ParentModel, ComponentModel]
  def viewModelLens: Lens[ParentViewModel, ComponentViewModel]

  def updateModel(
      context: SceneContext[StartupData],
      model: ParentModel,
      cmd: Command
  ): Outcome[ParentModel] =
    val n = nextModel(context, modelLens.get(model))(cmd)
    n.map(nn => modelLens.set(model, nn))

  def nextModel(
      context: SceneContext[StartupData],
      model: ComponentModel
  ): Command => Outcome[ComponentModel]

  def updateViewModel(
      context: SceneContext[StartupData],
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
      context: SceneContext[StartupData],
      model: ComponentModel,
      viewModel: ComponentViewModel
  ): Command => Outcome[ComponentViewModel]

  def present(
      context: SceneContext[StartupData],
      model: ParentModel,
      viewModel: ParentViewModel
  ): Outcome[Batch[SceneNode]] =
    view(
      context,
      modelLens.get(model),
      viewModelLens.get(viewModel)
    )

  def view(
      context: SceneContext[StartupData],
      model: ComponentModel,
      viewModel: ComponentViewModel
  ): Outcome[Batch[SceneNode]]
