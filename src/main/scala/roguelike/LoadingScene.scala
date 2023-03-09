package roguelike

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
import roguelike.model.GameLoadInfo
import roguelike.model.Loader
import roguelike.model.LoadingState
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.viewmodel.GameSprites
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel
import roguelike.viewmodel.ui.GameUi

object LoadingScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = GameLoadInfo
  type SceneViewModel = GameViewModel

  val name: SceneName =
    SceneName("loading scene")

  val modelLens: Lens[Model, GameLoadInfo] =
    Lens(_.loadInfo, (m, t) => m.copy(loadInfo = t))

  val viewModelLens: Lens[ViewModel, GameViewModel] =
    Lens(_.game, (vm, g) => vm.copy(game = g))

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set(AssetBundleLoader)

  def updateModel(
      context: SceneContext[Size],
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

        case _ =>
          Outcome(loadInfo)

    case StorageEvent.Loaded(ModelSaveData.saveKey, data) =>
      data match
        case None =>
          IndigoLogger.error("No saved data found")
          Outcome(loadInfo)

        case Some(d) =>
          ModelSaveData.fromJsonString(d) match
            case None =>
              IndigoLogger.error("Could not decode saved data...")
              Outcome(loadInfo)

            case sd @ Some(_) =>
              Outcome(loadInfo.copy(loadedData = sd))

    case AssetBundleLoaderEvent.LoadProgress(_, percent, _, _) if !loadInfo.state.isComplete =>
      Outcome(
        loadInfo.copy(
          state = LoadingState.InProgress(Some(percent))
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
      context: SceneContext[Size],
      loadInfo: GameLoadInfo,
      viewModel: GameViewModel
  ): GlobalEvent => Outcome[GameViewModel] =
    // case LoadEvent.SpritesLoaded(sprites) =>
    //   val player = sprites.find(_._1 == GameAssets.Player).map(_._2)

    //   val gameSprite: Option[GameSprites] =
    //     player.map { plr =>
    //       GameSprites(
    //         player = plr
    //       )
    //     }

    //   Outcome(
    //     viewModel.copy(
    //       sprites = gameSprite
    //     )
    //   )

    // case FrameTick if viewModel.sprites.isDefined && loadInfo.state.isComplete =>
    //   Outcome(viewModel).addGlobalEvents(SceneEvent.JumpTo(MainMenuScene.name))

    case _ =>
      Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      loadInfo: GameLoadInfo,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    // TODO Move all this inside the loader object
    val loader       = Loader(context, loadInfo.state)
    val textBounds   = loader.textBounds
    val loaderBounds = loader.getBounds(textBounds)
    val midX         = context.startUpData.width * 0.5
    val midY         = context.startUpData.height * 0.5

    Outcome(
      SceneUpdateFragment(
        Layer(
            // TODO Move all this inside the loaded
          loader
            .view(textBounds)
            .moveTo(
              (midX - (loaderBounds.width * 0.5)).toInt,
              (midY - (loaderBounds.height * 0.5)).toInt
            )
        )
      )
    )
