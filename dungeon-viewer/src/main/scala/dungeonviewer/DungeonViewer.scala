package dungeonviewer

import dungeonviewer.generated.Assets
import dungeonviewer.generated.Config
import indigo.*
import indigo.scenes.*
import indigoextras.subsystems.FPSCounter
import io.indigoengine.roguelike.starterkit.*

import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("IndigoGame")
object DungeonViewer extends IndigoGame[Unit, Unit, Model, ViewModel]:

  val screenSize = Size(80, 50)

  val maxTileCount: Int = 4000

  def initialScene(bootData: Unit): Option[SceneName] =
    Option(ViewerScene.name)

  def scenes(bootData: Unit): NonEmptyList[Scene[Unit, Model, ViewModel]] =
    NonEmptyList(ViewerScene)

  val eventFilters: EventFilters =
    EventFilters.Permissive

  def boot(flags: Map[String, String]): Outcome[BootResult[Unit]] =
    Outcome(
      BootResult
        .noData(Config.config)
        .withFonts(RoguelikeTiles.Size10x10.Fonts.fontInfo)
        .withAssets(Assets.assets.assetSet)
        .withShaders(
          TerminalEntity.shader(maxTileCount),
          TerminalText.standardShader
        )
    )

  def initialModel(startupData: Unit): Outcome[Model] =
    Outcome(Model.initial)

  def initialViewModel(startupData: Unit, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel.initial)

  def setup(bootData: Unit, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[Unit]] =
    Outcome(Startup.Success(()))

  def updateModel(context: FrameContext[Unit], model: Model): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  def updateViewModel(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
