package roguelike

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import indigo.syntax.animations.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.assets.GameAssets
import roguelike.game.GameScene
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.SceneTime
import roguelike.viewmodel.ViewModel

object MainMenuScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("main menu scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  val fadInTime      = Seconds(2)
  val slideInTime    = Seconds(5)
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
      Outcome(model)
        .addGlobalEvents(
          SceneEvent.JumpTo(GeneratingLevelScene.name),
          GenerateLevel
        )

    case KeyboardEvent.KeyUp(Key.KEY_C)
        if model.sceneTime.skip == true && model.loadInfo.loadedData.isDefined =>
      model.loadInfo.loadedData match
        case None =>
          Outcome(model) // should not happen...

        case Some(data) =>
          Outcome(Model.fromSaveData(data))
            .addGlobalEvents(
              SceneEvent.JumpTo(GameScene.name)
            )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Size],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        getTitle(context, model.sceneTime.skip)
      ).addLayer(getMenu(context, model))
    )

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
    val titleEnd = titleStart.moveTo(titleStart.x, 20)
    if (skipAnimations) Group(titleText.moveTo(titleEnd))
    else
      val titleAnimation: Timeline[Group] =
        timeline(
          layer(
            animate(4.seconds) {
              easeOut >>> lerp(titleStart, titleEnd) >>> moveGroup(_)
            }
          )
        )
      titleAnimation.at(context.running - context.sceneTime)(Group(titleText)) match {
        case Some(g) => g
        case None => Group.empty
      }

  def getMenu(context: SceneContext[Size], model: Model): Layer =
    val menuItems =
      getMenuFragment(
        context,
        context.startUpData.width * 0.5,
        !model.loadInfo.loadedData.isEmpty
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
      hasLoadData: Boolean
  ) =
    val menuItems = Batch(
      Text(
        "[n] Play a new game",
        RoguelikeTiles.Size10x10.Fonts.fontKey,
        TerminalText(GameAssets.TileMap, RGB.White, RGBA.Zero)
      )
    )
    val loadGameItem = Text(
      "[c] Continue last game",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.TileMap, RGB.White, RGBA.Zero)
    )
      .moveTo(0, 20)

    val menuMagnification = 2

    Layer(
      Group(
        (menuItems ++
          (if (hasLoadData)
             Batch(loadGameItem)
           else
             Batch.empty))
      ).withScale(new Vector2(menuMagnification, menuMagnification))
        .withPosition(
          new Point(
            (halfWidth - (context.boundaryLocator
              .textBounds(loadGameItem)
              .size
              .width * menuMagnification * 0.5)).toInt,
            200
          )
        )
    )
case object GenerateLevel extends GlobalEvent
