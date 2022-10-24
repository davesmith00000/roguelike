package roguelike

import indigo.*
import indigo.platform.assets.AssetLoader
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

  val spriteData = Batch(
    SpriteAssetData(GameAssets.Player, GameAssets.PlayerData),
    SpriteAssetData(GameAssets.Death, GameAssets.DeathData),
    SpriteAssetData(GameAssets.Enemy1, GameAssets.EnemyData1),
    SpriteAssetData(GameAssets.Enemy2, GameAssets.EnemyData2),
    SpriteAssetData(GameAssets.Enemy3, GameAssets.EnemyData3)
  )

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
      val missingAssets = spriteData
        .filterNot(s =>
          assetCollection.exists(s.imageData) && assetCollection.exists(
            s.jsonData
          )
        )
        .nonEmpty
      if (missingAssets)
        Outcome(
          Startup
            .Success(bootData)
        )
      else
        val spriteAnimationLoader =
          roguelike.AssetLoader.loadAnimation(assetCollection, dice)
        val spritesAndAnimations: Batch[(AssetName, SpriteAndAnimations)] =
          spriteData
            .map(s =>
              (
                s.imageData,
                spriteAnimationLoader(s.imageData, s.jsonData, Depth(1))
              )
            )
            .collect { case (s: AssetName, Right(v)) =>
              (s, v)
            }

        Outcome(
          Startup
            .Success(bootData)
            .addAnimations(
              spritesAndAnimations.map(s => s(1).animations).toList
            )
        )
          .addGlobalEvents(
            LoadEvent.SpritesLoaded(
              spritesAndAnimations.map(s => (s(0), s(1).sprite))
            )
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

private final case class SpriteAssetData(
    imageData: AssetName,
    jsonData: AssetName
)
