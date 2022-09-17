package roguelike

import indigo._
import indigo.scenes._
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
import roguelike.extensions.*
import roguelike.model.GameLoadInfo
import roguelike.model.LoadingState
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.model.SceneTime
import roguelike.viewmodel.ViewModel

object LogoScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = SceneTime
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("logo scene")

  val modelLens: Lens[Model, SceneTime] =
    Lens(_.sceneTime, (m, t) => m.copy(sceneTime = t))

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  val fadeTime     = Seconds(6)
  val stoppingTime = Seconds(2)
  val maxSceneTime = (fadeTime + stoppingTime) * 2

  def updateModel(
      context: FrameContext[Size],
      model: SceneTime
  ): GlobalEvent => Outcome[SceneTime] =
    case e: SceneEvent =>
      e match {
        case SceneEvent.SceneChange(_, _, _) =>
          Outcome(model.copy(time = Seconds(0), skip = false))
        case _ => Outcome(model)
      }
    case FrameTick =>
      if (model.time > maxSceneTime)
        Outcome(model)
          .addGlobalEvents(
            SceneEvent.JumpTo(LoadingScene.name)
          )
      else
        Outcome(model.copy(time = model.time + context.delta))
    case _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Size],
      model: SceneTime,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      model: SceneTime,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val halfWidth  = context.startUpData.width * 0.5
    val halfHeight = context.startUpData.height * 0.5
    val logo =
      if (model.time < (fadeTime + stoppingTime))
        getIndigoGraphic(model.time)
      else
        getPurpleKingdomGraphic(model.time)

    val logoHalfSize = logo.bounds.halfSize
    Outcome(
      SceneUpdateFragment(
        logo
          .moveTo(
            Point(
              (halfWidth - logoHalfSize.width).toInt,
              (halfHeight - logoHalfSize.height).toInt
            )
          )
      )
    )

  def getIndigoGraphic(time: Seconds) =
    val indigoLogoWidth  = 100
    val indigoLogoHeight = 105

    Graphic(
      Rectangle(0, 0, indigoLogoWidth, indigoLogoHeight),
      1,
      Material
        .Bitmap(GameAssets.IndigoLogo)
        .stretch
        .toImageEffects
        .fadeInOut(fadeTime, stoppingTime, time)
    ).withScale(Vector2(2, 2))

  def getPurpleKingdomGraphic(time: Seconds) =
    val purpleKingdomLogoWidth  = 100
    val purpleKingdomLogoHeight = 136

    Graphic(
      Rectangle(0, 0, purpleKingdomLogoWidth, purpleKingdomLogoHeight),
      1,
      Material
        .Bitmap(GameAssets.PurpleKingdomLogo)
        .stretch
        .toImageEffects
        .fadeInOut((fadeTime + stoppingTime), fadeTime, stoppingTime, time)
    ).withScale(Vector2(2, 2))
