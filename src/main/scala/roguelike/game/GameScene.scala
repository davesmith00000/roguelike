package roguelike.game

import indigo._
import indigo.scenes._
import roguelike.model.Model
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel

object GameScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = GameViewModel

  val name: SceneName =
    SceneName("game scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, GameViewModel] =
    Lens(_.game, (vm, gvm) => vm.copy(game = gvm))

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    GameSceneUpdate.updateModel(context, model)

  def updateViewModel(
      context: FrameContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): GlobalEvent => Outcome[GameViewModel] =
    viewModel.update(context, model)

  def present(
      context: FrameContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    GameView.present(context, model, viewModel)
