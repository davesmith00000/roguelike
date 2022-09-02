package roguelike

import indigo._
import indigo.scenes._
import io.indigoengine.roguelike.starterkit.*
import org.scalajs.dom.Worker
import roguelike.GameEvent
import roguelike.game.GameScene
import roguelike.model.Dungeon
import roguelike.model.DungeonGenConfig
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.js.JsDungeon
import roguelike.model.js.JsDungeonGameMapTuple
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
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case GenerateLevel =>
      Outcome(model).addGlobalEvents(
        workerSubSystem.WorkerEvent.Send(new DungeonGenConfig {
          val seed: Double      = context.dice.seed.toDouble
          val currentLevel: Int = model.currentFloor
        })
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
          GameEvent.Log(Message("Welcome!", RGB.Cyan))
        )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Size],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Text(
          "Generating level" + ("." * (context.gameTime.running.toInt % 4)),
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        )
      )
    )
