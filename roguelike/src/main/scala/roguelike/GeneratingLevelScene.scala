package roguelike

import indigo.*
import indigo.scenes.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.assets.GameAssets
import roguelike.game.GameScene
import roguelike.model.LoaderAnim
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.dungeon.DungeonGenConfig
import roguelike.model.js.JsDungeon
import roguelike.model.js.JsGameMap
import roguelike.subsystems.WorkerSubSystem
import roguelike.viewmodel.ViewModel
import roguelike.workers.DungeonGenWorker

object GeneratingLevelScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("generating level scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val workerSubSystem =
    WorkerSubSystem(DungeonGenWorker)
  val subSystems: Set[SubSystem] =
    Set(workerSubSystem)

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
          JsGameMap.toGameMap(msg.gameMap)
        )
        .map(_.copy(loadInfo = model.loadInfo))
        .addGlobalEvents(
          SceneEvent.JumpTo(GameScene.name),
          GameEvent.CameraSnapToPlayer,
          GameEvent.Log(Message("Welcome!", RGBA.Cyan))
        )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        LoaderAnim.present(context, "Generating level.", context.startUpData)
      )
    )
