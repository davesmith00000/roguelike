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

object LogoScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Unit
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("logo scene")

  val modelLens: Lens[Model, Unit] =
    Lens.unit

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  val fadeInTime   = Seconds(5)
  val stoppingTime = Seconds(2)
  val fadeOutTime  = Seconds(2)
  val maxSceneTime = (fadeInTime + stoppingTime + fadeOutTime) * 2
  val fadeSignal = Signal(t =>
    if (t <= fadeInTime)
      (t / fadeInTime).toDouble
    else if (t > fadeInTime + stoppingTime)
      Math.max(0, 1 - (t / (fadeInTime + stoppingTime + fadeOutTime)).toDouble)
    else
      1
  )

  def updateModel(
      context: FrameContext[Size],
      model: Unit
  ): GlobalEvent => Outcome[Unit] =
    case FrameTick =>
      if (context.running > maxSceneTime)
        Outcome(model)
          .addGlobalEvents(
            AssetBundleLoaderEvent.Load(
              BindingKey("Loading"),
              Assets.Game.assets
            ),
            StorageEvent.Load(ModelSaveData.saveKey),
            SceneEvent.JumpTo(MainMenuScene.name)
          )
      else
        Outcome(model)
    case _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Size],
      model: Unit,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      model: Unit,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val halfWidth  = context.startUpData.width * 0.5
    val halfHeight = context.startUpData.height * 0.5
    val logo =
      if (context.running < (fadeInTime + stoppingTime + fadeOutTime))
        getIndigoGraphic(context.running)
      else
        getPurpleKingdomGraphic(context.running)

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
        .Bitmap(Assets.Basic.indigoLogo)
        .stretch
        .toImageEffects
        .withAlpha(fadeSignal.at(time))
    ).withScale(Vector2(2, 2))

  def getPurpleKingdomGraphic(time: Seconds) =
    val purpleKingdomLogoWidth  = 100
    val purpleKingdomLogoHeight = 136

    Graphic(
      Rectangle(0, 0, purpleKingdomLogoWidth, purpleKingdomLogoHeight),
      1,
      Material
        .Bitmap(Assets.Basic.purpleKingdomLogo)
        .stretch
        .toImageEffects
        .withAlpha(
          fadeSignal.at(time - (fadeInTime + stoppingTime + fadeOutTime))
        )
    ).withScale(Vector2(2, 2))
