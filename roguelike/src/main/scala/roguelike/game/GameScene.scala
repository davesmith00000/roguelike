package roguelike.game

import indigo.*
import indigo.scenes.*
import roguelike.GameEvent
import roguelike.GenerateLevel
import roguelike.model.Model
import roguelike.model.dungeon.Dungeon
import roguelike.model.dungeon.DungeonGenConfig
import roguelike.model.js.JsDungeon
import roguelike.model.js.JsDungeonGameMapTuple
import roguelike.model.js.JsGameMap
import roguelike.subsystems.WorkerSubSystem
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel
import roguelike.workers.DungeonGenWorker

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

  val workerSubSystem =
    WorkerSubSystem(DungeonGenWorker)
  val subSystems: Set[SubSystem] =
    Set(workerSubSystem)

  val sprite: Set[Sprite[Material.ImageEffects]] = Set.empty

  def updateModel(
      context: SceneContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case GenerateLevel =>
      Outcome(model).addGlobalEvents(
        workerSubSystem.WorkerEvent.Send(
          DungeonGenConfig(
            context.dice.seed.toDouble,
            model.currentFloor
          )
        )
      )

    case workerSubSystem.WorkerEvent.Receive(msg) =>
      Model
        .assignDungeon(
          context.dice,
          JsDungeon.toDungeon(msg.dungeon),
          JsGameMap.toGameMap(msg.gameMap),
          Some(model)
        )
        .map(_.copy(loadInfo = model.loadInfo))
        .addGlobalEvents(
          SceneEvent.JumpTo(GameScene.name),
          GameEvent.CameraSnapToPlayer
        )

    case e =>
      GameSceneUpdate.updateModel(context, model)(e)

  def updateViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): GlobalEvent => Outcome[GameViewModel] =
    viewModel.update(context, model)

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    GameView.present(context, model, viewModel)
