package roguelike

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
import roguelike.model.GameLoadInfo
import roguelike.model.LoaderAnim
import roguelike.model.LoadingState
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.viewmodel.GameSprites
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel

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

  val loadBindingKey: BindingKey = BindingKey("initial game load")

  def updateModel(
      context: SceneContext[Size],
      loadInfo: GameLoadInfo
  ): GlobalEvent => Outcome[GameLoadInfo] =
    case FrameTick if loadInfo.state.hasNotStarted =>
      Outcome(GameLoadInfo.initial.start)
        .addGlobalEvents(
          AssetBundleLoaderEvent.Load(
            loadBindingKey,
            GameAssets.assets.game.assetSet
          ),
          StorageEvent.Load(ModelSaveData.saveKey)
        )

    case StorageEvent.Loaded(ModelSaveData.saveKey, Some(data)) =>
      ModelSaveData.fromJsonString(data) match
        case Left(msg) =>
          IndigoLogger.error(s"Could not decode saved data, got: $msg")
          Outcome(loadInfo)

        case Right(sd) =>
          IndigoLogger.info("Save data found and loaded")
          Outcome(loadInfo.withSaveData(sd))

    case StorageEvent.Loaded(ModelSaveData.saveKey, None) =>
      IndigoLogger.info("No saved data found")
      Outcome(loadInfo)

    case AssetBundleLoaderEvent.Started(key) if key == loadBindingKey =>
      IndigoLogger.info("Asset load started")
      Outcome(loadInfo)

    case AssetBundleLoaderEvent.LoadProgress(key, percent, _, _) if key == loadBindingKey =>
      IndigoLogger.info(s"Asset load progress ${percent}%")
      Outcome(
        loadInfo.toInProgress(percent)
      )

    case AssetBundleLoaderEvent.Success(key) if key == loadBindingKey =>
      IndigoLogger.info("Asset load success")
      Outcome(
        loadInfo.toComplete
      )

    case AssetBundleLoaderEvent.Failure(key, message) if key == loadBindingKey =>
      IndigoLogger.info("Asset load failure")
      Outcome(
        loadInfo.toError(message)
      )

    case _ =>
      Outcome(loadInfo)

  def updateViewModel(
      context: SceneContext[Size],
      loadInfo: GameLoadInfo,
      viewModel: GameViewModel
  ): GlobalEvent => Outcome[GameViewModel] =
    case LoadEvent.SpritesLoaded(sprites) =>
      val player = sprites.find(_._1 == GameAssets.assets.game.Player).map(_._2)
      val orc    = sprites.find(_._1 == GameAssets.assets.game.enemy1).map(_._2)
      val troll  = sprites.find(_._1 == GameAssets.assets.game.ENEMY2).map(_._2)

      val gameSprite: Option[GameSprites] =
        for {
          p <- player
          o <- orc
          t <- troll
        } yield GameSprites(p, o, t)

      Outcome(
        viewModel.copy(
          sprites = gameSprite
        )
      )

    case FrameTick if viewModel.sprites.isDefined && loadInfo.state.isComplete =>
      Outcome(viewModel).addGlobalEvents(SceneEvent.JumpTo(MainMenuScene.name))

    case _ =>
      Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      loadInfo: GameLoadInfo,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    val textCopy =
      loadInfo.state match
        case LoadingState.NotStarted =>
          "Started loading..."

        case LoadingState.InProgress(percent) =>
          "Loaded..." + (percent match
            case Some(p) => s"${p.toString}%"
            case None    => ""
          )

        case LoadingState.Complete =>
          "Loaded successfully!"

        case LoadingState.Error(msg) =>
          s"Error, some assets could not be loaded.\nError: $msg"

    Outcome(
      SceneUpdateFragment(
        LoaderAnim.present(context, textCopy, context.startUpData)
      )
    )
