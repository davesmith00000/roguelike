package roguelike

import indigo._
import indigo.scenes._
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
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

  val animationsKey: AnimationKey = AnimationKey("load")

  val animations: Set[Animation] = Set(
    Animation(
      animationsKey,
      Frame(Rectangle(0, 0, 32, 32), Millis(200)),
      Frame(Rectangle(32, 0, 32, 32), Millis(200)),
      Frame(Rectangle(64, 0, 32, 32), Millis(200)),
      Frame(Rectangle(96, 0, 32, 32), Millis(200))
    )
  )

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
                GameAssets.lazyAssets
              ),
              StorageEvent.Load(ModelSaveData.saveKey)
            )

        case LoadingState.InProgress(percent) =>
          Outcome(loadInfo.updateTimeout(context.delta))

        case LoadingState.Complete if loadInfo.isReallyComplete =>
          Outcome(loadInfo.updateTimeout(context.delta))
            .addGlobalEvents(SceneEvent.JumpTo(MainMenuScene.name))

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
    val midX = context.startUpData.width * 0.5
    val midY = context.startUpData.height * 0.5

    val graphic =
      Sprite(
        BindingKey("loading animation"),
        0,
        0,
        1,
        animationsKey,
        Material.Bitmap(GameAssets.Loader)
      ).play()
    val text =
      Text(
        loadInfo.state match
          case LoadingState.NotStarted =>
            "Started loading..."

          case LoadingState.InProgress(percent) =>
            s"Loaded...${percent.toString}%"

          case LoadingState.Complete =>
            "Loaded successfully!"

          case LoadingState.Error =>
            "Error, some assets could not be loaded."
        ,
        RoguelikeTiles.Size10x10.Fonts.fontKey,
        TerminalText(GameAssets.TileMap, RGB.White, RGBA.Zero)
      )
    val textBounds = context.boundaryLocator.textBounds(text)

    Outcome(
      SceneUpdateFragment(
        graphic.moveTo(
          (midX - 16).toInt,
          (midY - (textBounds.height * 0.5) - 32).toInt
        ),
        text.moveTo(
          (midX - (textBounds.width * 0.5)).toInt,
          (midY - (textBounds.height * 0.5)).toInt
        )
      )
    )
