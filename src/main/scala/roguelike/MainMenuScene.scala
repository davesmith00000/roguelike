package roguelike

import indigo._
import indigo.scenes._
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.extensions._
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

  def updateModel(
      context: FrameContext[Size],
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
      context: FrameContext[Size],
      model: Model,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: FrameContext[Size],
      model: Model,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val halfWidth         = context.startUpData.width * 0.5
    val halfHeight        = context.startUpData.height * 0.5
    val textMagnification = 3

    val titleText = Text(
      "My Generic Roguelite",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.Yellow, RGBA.Zero)
    )
    val titleTextBound =
      context.boundaryLocator.textBounds(titleText)
    val titleStart = Point(
      (halfWidth - (titleTextBound.size.width * textMagnification * 0.5)).toInt,
      (halfHeight - (titleTextBound.size.height * textMagnification * 0.5)).toInt
    )
    val titleEnd = titleStart.moveTo(titleStart.x, 20)

    Outcome(
      SceneUpdateFragment(
        Group(
          titleText
        )
          .withScale(new Vector2(textMagnification, textMagnification))
          .withPosition(titleStart)
          .easeOut(titleStart, titleEnd, Seconds(5), model.sceneTime.time),
        getMenuFragment(
          context,
          halfWidth,
          model.sceneTime.skip,
          !model.loadInfo.loadedData.isEmpty
        )
      )
    )

  def getMenuFragment(
      context: FrameContext[Size],
      halfWidth: Double,
      showMenu: Boolean,
      hasLoadData: Boolean
  ) =
    if (showMenu)
      val menuItems = Batch(
        Text(
          "[n] Play a new game",
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        )
      )
      val loadGameItem = Text(
        "[c] Continue last game",
        RoguelikeTiles.Size10x10.Fonts.fontKey,
        TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
      )
        .moveTo(0, 20)

      val menuMagnification = 2

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
    else Group.empty

case object GenerateLevel extends GlobalEvent
