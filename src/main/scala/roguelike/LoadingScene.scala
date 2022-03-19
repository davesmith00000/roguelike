package roguelike

import indigo._
import indigo.scenes._
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.GameLoadInfo
import roguelike.model.LoadingState
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
    Set(AssetBundleLoader)

  def updateModel(
      context: FrameContext[Size],
      loadInfo: GameLoadInfo
  ): GlobalEvent => Outcome[GameLoadInfo] =
    case FrameTick =>
      loadInfo.state match
        case LoadingState.NotStarted =>
          Outcome(GameLoadInfo.withTimeOut(Seconds(1.5)).start)
            .addGlobalEvents(
              AssetBundleLoaderEvent.Load(
                BindingKey("Loading"),
                Assets.Game.assets
              ),
              StorageEvent.Load(ModelSaveData.saveKey)
            )

        case LoadingState.InProgress(percent) =>
          Outcome(loadInfo.updateTimeout(context.delta))

        case LoadingState.Complete if loadInfo.isReallyComplete =>
          Outcome(loadInfo.updateTimeout(context.delta))
            .addGlobalEvents(SceneEvent.Next)

        case LoadingState.Complete =>
          Outcome(loadInfo.updateTimeout(context.delta))

        case LoadingState.Error =>
          Outcome(loadInfo)

    case StorageEvent.Loaded(ModelSaveData.saveKey, data) =>
      ModelSaveData.fromJsonString(data) match
        case None =>
          IndigoLogger.error("Could not decode saved data...")
          Outcome(loadInfo)

        case sd @ Some(_) =>
          Outcome(loadInfo.copy(loadedData = sd))

    case AssetBundleLoaderEvent.LoadProgress(_, percent, _, _) =>
      Outcome(
        loadInfo.copy(
          state = LoadingState.InProgress(percent)
        )
      )

    case AssetBundleLoaderEvent.Success(_) =>
      Outcome(
        loadInfo.copy(
          state = LoadingState.Complete
        )
      )

    case AssetBundleLoaderEvent.Failure(_, _) =>
      Outcome(
        loadInfo.copy(
          state = LoadingState.Error
        )
      )

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
    val text =
      loadInfo.state match
        case LoadingState.NotStarted =>
          "Started loading..."

        case LoadingState.InProgress(percent) =>
          s"Loaded...${percent.toString}%"

        case LoadingState.Complete =>
          "Loaded successfully!"

        case LoadingState.Error =>
          "Error, some assets could not be loaded."

    Outcome(
      SceneUpdateFragment(
        Text(
          text,
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        ).moveTo(10, 10)
      )
    )
