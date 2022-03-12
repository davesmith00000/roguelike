package roguelike

import indigo._
import indigo.scenes._
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.GameLoadInfo
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.viewmodel.ViewModel

object LoadingScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = GameLoadInfo
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("loading scene")

  val modelLens: Lens[Model, GameLoadInfo] =
    Lens(_.loadInfo, (m, t) => m.copy(loadInfo = t))

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: FrameContext[Size],
      loadInfo: GameLoadInfo
  ): GlobalEvent => Outcome[GameLoadInfo] =
    case FrameTick =>
      loadInfo.loadingTimeOut match
        case None =>
          // Attempt to load
          Outcome(GameLoadInfo(Option(Seconds(1.5)), None))
            .addGlobalEvents(StorageEvent.Load(ModelSaveData.saveKey))

        case t @ Some(timeRemaining) if timeRemaining.toDouble <= 0.0 =>
          // Give up!
          Outcome(loadInfo).addGlobalEvents(SceneEvent.Next)

        case Some(timeRemaining) =>
          Outcome(loadInfo.updateTimeout(context.delta))

    case StorageEvent.Loaded(ModelSaveData.saveKey, data) =>
      ModelSaveData.fromJsonString(data) match
        case None =>
          IndigoLogger.error("Could not decode saved data...")
          Outcome(loadInfo).addGlobalEvents(SceneEvent.Next)

        case sd @ Some(_) =>
          Outcome(loadInfo.copy(loadedData = sd))
            .addGlobalEvents(SceneEvent.Next)

    case _ =>
      Outcome(loadInfo)

  def updateViewModel(
      context: FrameContext[Size],
      loadInfo: GameLoadInfo,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      loadInfo: GameLoadInfo,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Text(
          "Loading...",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.tileMap, RGB.White, RGBA.Zero)
        )
          .moveTo(10, 10)
      )
    )
