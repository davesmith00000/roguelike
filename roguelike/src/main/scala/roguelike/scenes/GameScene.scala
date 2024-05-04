package roguelike.scenes

import dungeongen.js.DungeonGenConfig
import dungeongen.js.JsDungeon
import dungeongen.js.JsDungeonGameMapTuple
import dungeongen.js.JsGameMap
import dungeongen.workers.DungeonGenWorker
import indigo.*
import indigo.scenes.*
import roguelike.GameEvent
import roguelike.RogueLikeGame
import roguelike.model.GameWindowContext
import roguelike.model.GameWindows
import roguelike.model.Model
import roguelike.model.dungeon.Dungeon
import roguelike.subsystems.WorkerSubSystem
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel
import roguelike.windows.HistoryWindow
import roguelike.windows.InfoPanel
import roguelike.windows.MenuWindow
import roguelikestarterkit.Dimensions
import roguelikestarterkit.ui.window.WindowManager

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

  val subSystems: Set[SubSystem[Model]] =
    Set(
      workerSubSystem,
      WindowManager[Model, GameWindowContext](
        SubSystemId("window manager"),
        1, // TODO: Needs to propogate into the windows.
        GameWindows.defaultCharSheet,
        _.gameWindowContext
      )
        .withLayerKey(RogueLikeGame.layerKeyWindows)
        .register(
          InfoPanel.window(GameWindows.defaultCharSheet)
        )
        .register(
          HistoryWindow.window(GameWindows.defaultCharSheet)
        )
        .register(
          MenuWindow.window(
            Dimensions(1280, 720) / 10,
            GameWindows.defaultCharSheet
          )
        )
        .open(
          InfoPanel.windowId,
          // HistoryWindow.windowId,
          MenuWindow.windowId
        )
    )

  val sprite: Set[Sprite[Material.ImageEffects]] = Set.empty

  def updateModel(
      context: SceneContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case GameEvent.GenerateLevel =>
      Outcome(model).addGlobalEvents(
        workerSubSystem.WorkerEvent.Send(
          DungeonGenConfig(
            context.dice.seed.toDouble,
            model.currentFloor,
            RogueLikeGame.screenSize.width,
            RogueLikeGame.screenSize.height
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
