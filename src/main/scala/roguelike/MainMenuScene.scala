package roguelike

import indigo._
import indigo.scenes._
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.extensions._
import roguelike.game.GameScene
import roguelike.model.MainMenuModel
import roguelike.model.Message
import roguelike.model.Model
import roguelike.viewmodel.ViewModel

object MainMenuScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = MainMenuModel
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("main menu scene")

  val modelLens: Lens[Model, MainMenuModel] =
    Lens(
      m => new MainMenuModel(m, Seconds(0), false),
      (m, t) => t.gameData
    )

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  val fadInTime      = Seconds(2)
  val slideInTime    = Seconds(5)
  val titlePauseTime = Seconds(2)
  val menuFadeInTime = Seconds(2)
  val totalTime      = slideInTime + titlePauseTime + menuFadeInTime

  def updateModel(
      context: FrameContext[Size],
      model: MainMenuModel
  ): GlobalEvent => Outcome[MainMenuModel] =
    case FrameTick =>
      if (model.sceneTime < totalTime && model.showMenu == false)
        Outcome(model.copy(sceneTime = model.sceneTime + context.delta))
      else if (model.sceneTime >= totalTime)
        Outcome(model.copy(showMenu = true))
      else
        Outcome(model)

    case KeyboardEvent.KeyDown(_) if model.showMenu == false =>
      Outcome(model.copy(showMenu = true))

    case KeyboardEvent.KeyUp(Key.KEY_N) if model.showMenu == true =>
      Outcome(model)
        .addGlobalEvents(
          SceneEvent.JumpTo(GeneratingLevelScene.name),
          GenerateLevel
        )

    case KeyboardEvent.KeyUp(Key.KEY_C)
        if model.showMenu == true && model.gameData.loadInfo.loadedData.isDefined =>
      model.gameData.loadInfo.loadedData match
        case None =>
          Outcome(model) // should not happen...

        case Some(data) =>
          Outcome(model.copy(gameData = Model.fromSaveData(data)))
            .addGlobalEvents(
              SceneEvent.JumpTo(GameScene.name)
            )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: FrameContext[Size],
      model: MainMenuModel,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      model: MainMenuModel,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val loadColor: RGB =
      if model.gameData.loadInfo.loadedData.isEmpty then
        RGB.White.mix(RGB.Black, 0.5)
      else RGB.White

    Outcome(
      SceneUpdateFragment(
        Text(
          "My Generic Roguelite",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.Yellow, RGBA.Zero)
        )
          .lerp(Point(0, 0), Point(0, 10), Seconds(5), context.gameTime.running)
      )
    )

/*Outcome(
      SceneUpdateFragment(
        Text(
          "My Generic Roguelite",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.Yellow, RGBA.Zero)
        )
          .moveTo(10, 10),
        Text(
          "[n] Play a new game",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        )
          .moveTo(10, 25),
        Text(
          "[c] Continue last game",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, loadColor, RGBA.Zero)
        )
          .moveTo(10, 35)
      )
    )*/

case object GenerateLevel extends GlobalEvent
