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
import roguelike.subsystems.WorkerSubSystem
import roguelike.subsystems.WorkerName
import roguelike.viewmodel.ViewModel

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

  val subSystems: Set[SubSystem] =
    Set(
      WorkerSubSystem[DungeonGenConfig, Dungeon](
        WorkerName("dungeon-gen-worker")
      )
    )

  def updateModel(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case GenerateLevel =>
      Model
        .gen(context.dice)
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
          "Generating level...",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        )
      )
    )
