package roguelike

import indigo.*
import indigo.scenes.*
import indigoextras.subsystems.FPSCounter
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
import roguelike.game.GameScene
import roguelike.model.Model
import roguelike.screeneffects.InnerGlow
import roguelike.subsystems.FloatingMessage
import roguelike.viewmodel.ViewModel

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object RogueLikeGame extends IndigoGame[Size, Size, Model, ViewModel]:

  val screenSize: Size = Size(80, 50)
  val charSize: Size   = Size(10, 10)

  val layerKeyGame      = BindingKey("game")
  val layerKeyUiOverlay = BindingKey("ui-overlay")
  val layerKeyUi        = BindingKey("ui")
  val layerKeyFPS       = BindingKey("fps")

  def initialScene(bootData: Size): Option[SceneName] =
    None

  def scenes(bootData: Size): NonEmptyList[Scene[Size, Model, ViewModel]] =
    NonEmptyList(
      LogoScene,
      LoadingScene,
      MainMenuScene,
      GeneratingLevelScene,
      GameScene
    )

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[Size]] =
    val gameViewport =
      (flags.get("width"), flags.get("height")) match {
        case (Some(w), Some(h)) =>
          GameViewport(w.toInt, h.toInt)

        case _ =>
          val vp = screenSize * charSize
          GameViewport(vp.width, vp.height)
      }

    Outcome(
      BootResult(
        GameConfig.default
          .withMagnification(1)
          .withViewport(gameViewport),
        gameViewport.size
      )
        .withFonts(RoguelikeTiles.Size10x10.Fonts.fontInfo)
        .withAssets(GameAssets.initialAssets)
        .withShaders(TerminalText.standardShader, InnerGlow.shader)
        .withSubSystems(
          FPSCounter(
            Point(5, 100),
            layerKeyFPS
          ),
          FloatingMessage.subSystem
        )
    )

  def initialModel(startupData: Size): Outcome[Model] =
    Outcome(Model.blank(Dice.fromSeed(0))).addGlobalEvents()

  def initialViewModel(startupData: Size, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel.initial(model.player, startupData))

  def setup(
      bootData: Size,
      assetCollection: AssetCollection,
      dice: Dice
  ): Outcome[Startup[Size]] =
    if GameAssets.loaded(assetCollection) then
      Outcome(
        Startup
          .Success(bootData)
      )
    else Outcome(Startup.Success(bootData))

  def updateModel(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Size],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        Layer(layerKeyGame),
        Layer(layerKeyUiOverlay),
        Layer(layerKeyUi),
        Layer(layerKeyFPS)
      )
    )
