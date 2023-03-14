package roguelike

import indigo.*
import indigo.scenes.*
import indigo.shared.datatypes.Fill.Color.apply
import indigo.syntax.*
import indigo.syntax.animations.*
import indigoextras.ui.Button
import indigoextras.ui.HitArea
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.assets.GameAssets
import roguelike.game.GameScene
import roguelike.model.Message
import roguelike.model.Model
import roguelike.screeneffects.InnerGlow
import roguelike.viewmodel.ViewModel
import roguelike.viewmodel.ui.MainMenuUi

object MainMenuScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = MainMenuUi

  val name: SceneName =
    SceneName("main menu scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, MainMenuUi] =
    Lens(
      _.mainMenu,
      (vm, mainMenu) => vm.copy(mainMenu = mainMenu)
    )

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: SceneContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case NewGame =>
      Outcome(model)
        .addGlobalEvents(
          SceneEvent.JumpTo(GeneratingLevelScene.name),
          GenerateLevel
        )

    case LoadGame =>
      model.loadInfo.loadedData match
        case None =>
          Outcome(model)

        case Some(data) =>
          Outcome(Model.fromSaveData(model, data))
            .addGlobalEvents(
              SceneEvent.JumpTo(GameScene.name)
            )

    case KeyboardEvent.KeyUp(Key.KEY_N) =>
      Outcome(model).addGlobalEvents(NewGame)

    case KeyboardEvent.KeyUp(Key.KEY_C) =>
      Outcome(model).addGlobalEvents(LoadGame)

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: MainMenuUi
  ): GlobalEvent => Outcome[MainMenuUi] =
    case FrameTick =>
      val buttonSize        = viewModel.newGame.width
      val halfWidth         = context.startUpData.width * 0.5
      val menuMagnification = 2

      val mainMenu = {
        if (model.loadInfo.loadedData.isDefined && viewModel.loadGame.isEmpty)
          MainMenuUi(Batch(NewGame), Some(Batch(LoadGame)))
        else
          viewModel
      }
        .withScale(menuMagnification)
        .moveTo(
          new Point(
            (halfWidth - (buttonSize * menuMagnification * 0.5)).toInt,
            200
          )
        )

      val newGame = mainMenu.newGame.update(context.mouse)

      Outcome(mainMenu).merge(newGame)((vm, ng) => vm.copy(newGame = ng))

    case _ =>
      Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: MainMenuUi
  ): Outcome[SceneUpdateFragment] =
    val time            = context.sceneRunning
    val viewportSize    = context.startUpData
    val boundaryLocator = context.boundaryLocator

    Outcome(
      SceneUpdateFragment.empty
        .addLayer(
          Layer(MainMenuHelper.getBackground(time, viewportSize))
            .withBlendMaterial(InnerGlow(viewportSize, RGBA(0, 0, 0, 0.5), 0.4))
        )
        .addLayer(MainMenuHelper.getMenu(time, viewportSize, model, viewModel))
        .addLayer(MainMenuHelper.getTitle(time, viewportSize, boundaryLocator))
        .withAudio(MainMenuHelper.getBackgroundAudio(time))
    )

object MainMenuHelper:

  val slideInTime    = Seconds(4)
  val titlePauseTime = Seconds(1)
  val menuFadeInTime = Seconds(2)
  val totalTime      = slideInTime + titlePauseTime + menuFadeInTime

  val moveGroup: Group => SignalFunction[Point, Group] = g =>
    SignalFunction(pt => g.withPosition(pt))

  val applyAlpha: Layer => SignalFunction[Double, Layer] = l =>
    SignalFunction { d =>
      val material = l.blending.map(b => b.blendMaterial) match
        case Some(m) => m
        case None    => BlendMaterial.BlendEffects.None

      material match
        case m: BlendMaterial.BlendEffects =>
          l.withBlendMaterial(m.withAlpha(d))
        case _ => l

    }

  val changeVolume: Track => SignalFunction[Double, Track] = t =>
    SignalFunction(d => t.copy(volume = Volume.Max * d))

  def getBackground(time: Seconds, viewportSize: Size): Graphic[Material.ImageEffects] =
    val graphic = Graphic(
      viewportSize,
      Material
        .Bitmap(GameAssets.MenuBg)
        .tile
        .toImageEffects
        .withAlpha(0)
    )

    val graphicTimeline: Timeline[Graphic[Material.ImageEffects]] = timeline(
      layer(
        animate(1.seconds) { g =>
          lerp >>> SignalFunction { d =>
            g.modifyMaterial(_.withAlpha(d))
          }
        }
      )
    )

    if (time >= 1.second)
      graphic.modifyMaterial(_.withAlpha(1))
    else
      graphicTimeline.at(time)(graphic) match {
        case Some(g) => g
        case None    => graphic
      }

  def getTitle(time: Seconds, viewportSize: Size, boundaryLocator: BoundaryLocator): Group =
    // TODO: No need for the separate components.
    val halfWidth         = viewportSize.width * 0.5
    val halfHeight        = viewportSize.height * 0.5
    val textMagnification = 3

    val titleText = Text(
      "My Generic Roguelite",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.TileMap, RGB.Yellow, RGBA.Zero)
    )
    val titleTextBound =
      boundaryLocator.textBounds(titleText)
    val titleStart = Point(
      (halfWidth - (titleTextBound.size.width * textMagnification * 0.5)).toInt,
      (halfHeight - (titleTextBound.size.height * textMagnification * 0.5)).toInt
    )

    val group =
      Group(titleText)
        .withScale(new Vector2(textMagnification, textMagnification))
        .withPosition(titleStart)

    val titleEnd = titleStart.moveTo(titleStart.x, 60)

    val titleAnimation: Timeline[Group] =
      timeline(
        layer(
          animate(slideInTime) {
            easeOut >>> lerp(titleStart, titleEnd) >>> moveGroup(_)
          }
        )
      )
    titleAnimation.at(time)(group) match {
      case Some(g) => g
      case None    => Group.empty
    }

  def getMenu(time: Seconds, viewportSize: Size, model: Model, viewModel: MainMenuUi): Layer =

    val menuItems =
      getMenuFragment(
        viewportSize.width * 0.5,
        viewModel
      )

    val menuAnimation: Timeline[Layer] =
      timeline(
        layer(
          startAfter[Layer](6.seconds),
          animate(2.seconds)(lerp >>> applyAlpha(_))
        )
      )

    menuAnimation.at(time)(
      menuItems.withBlendMaterial(BlendMaterial.BlendEffects(0))
    ) match {
      case Some(l) => l
      case None    => Layer.empty
    }

  def getMenuFragment(
      halfWidth: Double,
      viewModel: MainMenuUi
  ): Layer =
    Layer(Group(viewModel.view))

  def getBackgroundAudio(time: Seconds): SceneAudio =
    val track = Track(GameAssets.MenuBackgroundAudio)
    val soundTimeline: Timeline[Track] =
      timeline(
        layer(
          animate(1.seconds)(lerp >>> changeVolume(_))
        )
      )

    SceneAudio(
      SceneAudioSource(
        BindingKey(GameAssets.MenuBackgroundAudio.toString),
        PlaybackPattern.SingleTrackLoop(
          soundTimeline.at(time)(track) match {
            case Some(t) => t
            case None    => track
          }
        )
      )
    )

case object GenerateLevel extends GlobalEvent
case object NewGame       extends GlobalEvent
case object LoadGame      extends GlobalEvent
