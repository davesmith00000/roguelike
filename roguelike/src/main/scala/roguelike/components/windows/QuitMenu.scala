package roguelike.components.windows

import indigo.*
import indigo.scenes.SceneContext
import indigo.scenes.SceneEvent
import roguelike.MainMenuScene
import roguelike.assets.GameAssets
import roguelike.components.Component
import roguelike.model.GameLoadInfo
import roguelike.model.GameState
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.viewmodel.GameViewModel
import roguelikestarterkit.*

object QuitMenu extends Component[Size, Model, GameViewModel]:
  type Command            = HandleInput
  type ComponentModel     = Model
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Model]            = Lens.identity
  def viewModelLens: Lens[GameViewModel, Size] = Lens.readOnly(_.viewportSize)

  def nextModel(
      context: SceneContext[Size],
      model: Model
  ): HandleInput => Outcome[Model] =
    // Save
    case HandleInput(Key.KEY_1) if model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(
        model.copy(loadInfo = model.loadInfo.withSaveData(saveData)),
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
        model.copy(loadInfo = model.loadInfo.withSaveData(saveData)),
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
      context: SceneContext[Size],
      model: Model,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      model: Model,
      viewportSize: Size
  ): Outcome[Batch[SceneNode]] =
    val text =
      s"""Quit to main menu?
          |
          |[1] Save
          |[2] Save and Quit
          |[3] Quit
          |""".stripMargin

    val windowSize = Size(300, 150)

    Outcome(
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
            TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)
      )
    )

  final case class HandleInput(key: Key)
