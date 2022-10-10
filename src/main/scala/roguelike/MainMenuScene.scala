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
import roguelike.model.SceneTime
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
    Lens({_.ui match {
      case m: MainMenuUi => m
      case _ => MainMenuUi(Batch(NewGame))
    }}, (vm, mainMenu) => vm.copy(ui = mainMenu))

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  val fadInTime      = Seconds(2)
  val slideInTime    = Seconds(4)
  val titlePauseTime = Seconds(1)
  val menuFadeInTime = Seconds(2)
  val totalTime      = slideInTime + titlePauseTime + menuFadeInTime
  val moveGroup: Group => SignalFunction[Point, Group] = g =>
    SignalFunction(pt => g.withPosition(pt))

  val applyAlpha: Layer => SignalFunction[Double, Layer] = l =>
    SignalFunction( d =>
      val material = l.blending.map(b => b.blendMaterial) match {
        case Some(m) => m
        case None    => BlendMaterial.BlendEffects.None
      }

      material match {
        case m: BlendMaterial.BlendEffects =>
          l.withBlendMaterial(m.withAlpha(d))
        case _ => l
      }
    )

  def updateModel(
      context: SceneContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case e: SceneEvent =>
      e match {
        case SceneEvent.SceneChange(_, _, _) =>
          Outcome(model.copy(sceneTime = SceneTime(Seconds(0), skip = false)))
        case _ => Outcome(model)
      }
    case NewGame =>
      Outcome(model)
        .addGlobalEvents(
          SceneEvent.JumpTo(GeneratingLevelScene.name),
          GenerateLevel
        )

    case LoadGame if model.loadInfo.loadedData.isDefined =>
      model.loadInfo.loadedData match
        case None =>
          Outcome(model) // should not happen...

        case Some(data) =>
          Outcome(Model.fromSaveData(data))
            .addGlobalEvents(
              SceneEvent.JumpTo(GameScene.name)
            )
    case FrameTick =>
      if (model.sceneTime.time < totalTime && model.sceneTime.skip == false)
        Outcome(
          model.copy(sceneTime =
            model.sceneTime.copy(time = model.sceneTime.time + context.delta)
          )
        )
      else if (model.sceneTime.time >= totalTime)
        Outcome(model.copy(sceneTime = model.sceneTime.copy(skip = true)))
      else
        Outcome(model)

    case KeyboardEvent.KeyDown(_) if model.sceneTime.skip == false =>
      Outcome(model.copy(sceneTime = model.sceneTime.copy(skip = true)))

    case KeyboardEvent.KeyUp(Key.KEY_N) if model.sceneTime.skip == true =>
      Outcome(model).addGlobalEvents(NewGame)

    case KeyboardEvent.KeyUp(Key.KEY_C) if model.sceneTime.skip == true =>
      Outcome(model).addGlobalEvents(LoadGame)
    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: MainMenuUi
  ): GlobalEvent => Outcome[MainMenuUi] =
    case FrameTick =>
      val buttonSize = viewModel.newGame.width
      val halfWidth = context.startUpData.width * 0.5
      val menuMagnification = 2
      val mainMenu =
      (
        if (!model.loadInfo.loadedData.isEmpty && viewModel.loadGame == None)
          MainMenuUi(Batch(NewGame), Some(Batch(LoadGame)))
        else
          viewModel
      ).moveTo(
        new Point(
          (halfWidth - (buttonSize * menuMagnification * 0.5)).toInt,
          200
        )
      )
        .withScale(menuMagnification)

      val newGame = mainMenu.newGame.update(context.mouse)
      Outcome(mainMenu).merge(newGame)((vm, ng) => vm.copy(newGame = ng))
    case _ =>
      Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: MainMenuUi
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment.empty
        .addLayer(
          Layer(getBackground(context))
            .withBlendMaterial(InnerGlow(context.startUpData, RGBA(0, 0, 0, 0.5), 0.4))
        )
        .addLayer(getMenu(context, model, viewModel))
        .addLayer(getTitle(context, model.sceneTime.skip))
    )

  def getBackground(context: SceneContext[Size]): Graphic[Material.ImageEffects] =
    val graphic = Graphic(
      context.startUpData,
      Material
        .Bitmap(GameAssets.MenuBg)
        .tile
        .toImageEffects
        .withAlpha(0)
    )

    val time = context.running - context.sceneTime
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
        case None => graphic
      }

  def getTitle(context: SceneContext[Size], skipAnimations: Boolean): Group =
    val halfWidth         = context.startUpData.width * 0.5
    val halfHeight        = context.startUpData.height * 0.5
    val textMagnification = 3

    val titleText = Text(
      "My Generic Roguelite",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.TileMap, RGB.Yellow, RGBA.Zero)
    )
    val titleTextBound =
      context.boundaryLocator.textBounds(titleText)
    val titleStart = Point(
      (halfWidth - (titleTextBound.size.width * textMagnification * 0.5)).toInt,
      (halfHeight - (titleTextBound.size.height * textMagnification * 0.5)).toInt
    )

    val group =
      Group(titleText)
        .withScale(new Vector2(textMagnification, textMagnification))
        .withPosition(titleStart)

    val time = context.running - context.sceneTime
    val titleEnd = titleStart.moveTo(titleStart.x, 60)

    if (skipAnimations || time >= slideInTime) group.moveTo(titleEnd)
    else
      val titleAnimation: Timeline[Group] =
        timeline(
          layer(
            animate(slideInTime) {
              easeOut >>> lerp(titleStart, titleEnd) >>> moveGroup(_)
            }
          )
        )
      titleAnimation.at(context.running - context.sceneTime)(group) match {
        case Some(g) => g
        case None => Group.empty
      }

  def getMenu(context: SceneContext[Size], model: Model, viewModel: MainMenuUi): Layer =
    val menuItems =
      getMenuFragment(
        context,
        context.startUpData.width * 0.5,
        viewModel
      )
    if (model.sceneTime.skip) menuItems.withBlendMaterial(BlendMaterial.BlendEffects.None)
    else
      val menuAnimation: Timeline[Layer] =
        timeline(
          layer(
            startAfter[Layer](6.seconds),
            animate(2.seconds){ lerp >>> applyAlpha(_) }
          )
        )
      menuAnimation.at(context.running - context.sceneTime)(menuItems.withBlendMaterial(BlendMaterial.BlendEffects(0))) match {
        case Some(l) => l
        case None => Layer.empty
      }

  def getMenuFragment(
      context: SceneContext[Size],
      halfWidth: Double,
      viewModel: MainMenuUi
  ) = Layer(Group(viewModel.view(context)))

case object GenerateLevel extends GlobalEvent
case object NewGame extends GlobalEvent
case object LoadGame extends GlobalEvent
