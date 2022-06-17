package roguelike.components.windows

import indigo.*
import indigo.scenes.Lens
import indigo.scenes.SceneEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.MainMenuScene
import roguelike.components.Component
import roguelike.model.GameLoadInfo
import roguelike.model.GameState
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.viewmodel.GameViewModel

object QuitMenu extends Component[Size, Model, GameViewModel]:
  type Command            = HandleInput
  type ComponentModel     = Model
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Model]            = Lens.identity
  def viewModelLens: Lens[GameViewModel, Size] = Lens.readOnly(_.viewportSize)

  def nextModel(
      context: FrameContext[Size],
      model: Model
  ): HandleInput => Outcome[Model] =
    // Save
    case HandleInput(Key.KEY_1) if model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(
        model.copy(loadInfo = GameLoadInfo.withSaveData(saveData)),
        Batch(
          StorageEvent.Save(
            ModelSaveData.saveKey,
            model.toSaveData.toJsonString
          )
        )
      )

    // Save and Quit
    case HandleInput(Key.KEY_2) if model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(
        model.copy(loadInfo = GameLoadInfo.withSaveData(saveData)),
        Batch(
          StorageEvent.Save(ModelSaveData.saveKey, saveData.toJsonString),
          SceneEvent.JumpTo(MainMenuScene.name)
        )
      )

    // Quit
    case HandleInput(Key.KEY_3) =>
      Outcome(model, Batch(SceneEvent.JumpTo(MainMenuScene.name)))

    case HandleInput(_) =>
      Outcome(model)

  def nextViewModel(
      context: FrameContext[Size],
      model: Model,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      model: Model,
      viewportSize: Size
  ): Batch[SceneNode] =
    val text =
      s"""Quit to main menu?
          |
          |[1] Save
          |[2] Save and Quit
          |[3] Quit
          |""".stripMargin

    val windowSize = Size(300, 150)

    Batch(
      Group(
        Shape.Box(
          Rectangle(Point.zero, windowSize),
          Fill.Color(RGBA.Black),
          Stroke(2, RGBA.Blue)
        ),
        Text(
          text,
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        )
          .moveTo(5, 5)
      ).moveTo(((viewportSize - windowSize) / 2).toPoint)
    )

  final case class HandleInput(key: Key)
