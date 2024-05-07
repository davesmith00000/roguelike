package roguelike.windows

import indigo.*
import indigo.scenes.SceneEvent
import roguelike.GameEvent
import roguelike.model.GameWindowContext
import roguelike.scenes.MainMenuScene
import roguelikestarterkit.*

object QuitSaveWindow:

  val windowId: WindowId = WindowId("Quit and Save")

  private val windowSize = Dimensions(26, 22)

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[QuitMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      QuitMenu.initial(charSheet)
    )
      .withTitle("Quit & Save")
      .moveTo(((screenSize - windowSize) / 2).toCoords)
      .resizeTo(windowSize)

final case class QuitMenu(components: ComponentGroup)
object QuitMenu:

  def initial(charSheet: CharSheet): QuitMenu =
    val buttonTheme =
      Button.Theme(
        charSheet,
        RGBA.Silver -> RGBA.Black,
        RGBA.White  -> RGBA.Black,
        RGBA.Black  -> RGBA.White
      )

    QuitMenu(
      ComponentGroup(Bounds(0, 0, 16, 1))
        .withLayout(ComponentLayout.Vertical())
        .add(
          Button(
            "[1] Save",
            buttonTheme
          ).onClick(GameEvent.SaveGame),
          Button(
            "[2] Save and Quit",
            buttonTheme
          ).onClick(
            GameEvent.SaveGame,
            GameEvent.WindowEvent(WindowManagerCommand.CloseAll),
            SceneEvent.JumpTo(MainMenuScene.name)
          ),
          Button(
            "[3] Quit",
            buttonTheme
          ).onClick(
            GameEvent.WindowEvent(WindowManagerCommand.CloseAll),
            SceneEvent.JumpTo(MainMenuScene.name)
          )
        )
    )

  given WindowContent[QuitMenu, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: QuitMenu
    ): GlobalEvent => Outcome[QuitMenu] =

      // Save
      case KeyboardEvent.KeyUp(Key.KEY_1) =>
        Outcome(model, Batch(GameEvent.SaveGame))

      // Save and Quit
      case KeyboardEvent.KeyUp(Key.KEY_2) =>
        Outcome(model, Batch(GameEvent.SaveGame, SceneEvent.JumpTo(MainMenuScene.name)))

      // Quit
      case KeyboardEvent.KeyUp(Key.KEY_3) =>
        Outcome(model, Batch(SceneEvent.JumpTo(MainMenuScene.name)))

      case e =>
        model.components.update(context)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UiContext[GameWindowContext],
        model: QuitMenu
    ): Outcome[Layer] =
      model.components.present(context).map(_.toLayer)

    def cascade(model: QuitMenu, newBounds: Bounds): QuitMenu =
      model.copy(components = model.components.cascade(newBounds))
