package roguelike

import indigo._
import indigo.scenes._
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.game.GameScene
import roguelike.model.Message
import roguelike.model.Model
import roguelike.viewmodel.ViewModel

object MainMenuScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("main menu scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case KeyboardEvent.KeyUp(Key.KEY_N) =>
      Model
        .gen(context.dice, RogueLikeGame.screenSize)
        .map(_.copy(loadInfo = model.loadInfo))
        .addGlobalEvents(
          SceneEvent.JumpTo(GameScene.name),
          GameEvent.CameraSnapToPlayer,
          GameEvent.Log(Message("Welcome!", RGB.Cyan))
        )

    case KeyboardEvent.KeyUp(Key.KEY_C)
        if model.loadInfo.loadedData.isDefined =>
      model.loadInfo.loadedData match
        case None =>
          Outcome(model) // should not happen...

        case Some(data) =>
          Outcome(Model.fromSaveData(data))
            .addGlobalEvents(
              SceneEvent.JumpTo(GameScene.name)
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
    val loadColor: RGB =
      if model.loadInfo.loadedData.isEmpty then RGB.White.mix(RGB.Black, 0.5)
      else RGB.White

    Outcome(
      SceneUpdateFragment(
        Text(
          "My Generic Roguelite",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.tileMap, RGB.Yellow, RGBA.Zero)
        )
          .moveTo(10, 10),
        Text(
          "[n] Play a new game",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.tileMap, RGB.White, RGBA.Zero)
        )
          .moveTo(10, 25),
        Text(
          "[c] Continue last game",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.tileMap, loadColor, RGBA.Zero)
        )
          .moveTo(10, 35)
      )
    )
