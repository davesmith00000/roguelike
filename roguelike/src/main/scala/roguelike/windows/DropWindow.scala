package roguelike.windows

import indigo.*
import roguelike.model.GameWindowContext
import roguelike.scenes.UIElements
import roguelikestarterkit.*

object DropWindow:

  val windowId: WindowId = WindowId("Drop")

  private val windowSize = Dimensions(26, 22)

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[DropMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      DropMenu.initial(charSheet)
    )
      .withTitle("Drop Item")
      .moveTo(((screenSize - windowSize) / 2).toCoords)
      .resizeTo(windowSize)

final case class DropMenu(components: ComponentGroup[GameWindowContext])
object DropMenu:

  def initial(charSheet: CharSheet): DropMenu =
    DropMenu(
      ComponentGroup(Bounds(0, 0, 16, 1))
        .withLayout(ComponentLayout.Vertical())
        .add(
          Label(
            (ctx: GameWindowContext) =>
              if ctx.player.inventory.backpack.count > 0 then
                val collectables: String =
                  ctx.player.inventory.backpack.items
                    .map(_.name)
                    .zip(UIElements.letters)
                    .foldLeft(("", 0)) { case ((str, r), (collectableName, letter)) =>
                      (str + s"[$letter] $collectableName\n", r + 1)
                    }
                    ._1

                "Select a collectable to drop\n" + collectables
              else s"""Select an collectable to drop
                |
                |(Empty)
                |""".stripMargin,
            Label.Theme(charSheet)
          )
        )
        // .add(
        //   Button(
        //     "[1] Save",
        //     buttonTheme
        //   ).onClick(GameEvent.SaveGame),
        //   Button(
        //     "[2] Save and Quit",
        //     buttonTheme
        //   ).onClick(
        //     GameEvent.SaveGame,
        //     GameEvent.WindowEvent(WindowManagerCommand.CloseAll),
        //     SceneEvent.JumpTo(MainMenuScene.name)
        //   ),
        //   Button(
        //     "[3] Quit",
        //     buttonTheme
        //   ).onClick(
        //     GameEvent.WindowEvent(WindowManagerCommand.CloseAll),
        //     SceneEvent.JumpTo(MainMenuScene.name)
        //   )
        // )
    )

  given WindowContent[DropMenu, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: DropMenu
    ): GlobalEvent => Outcome[DropMenu] =

      // // Save
      // case KeyboardEvent.KeyUp(Key.KEY_1) =>
      //   Outcome(model, Batch(GameEvent.SaveGame))

      // // Save and Quit
      // case KeyboardEvent.KeyUp(Key.KEY_2) =>
      //   Outcome(model, Batch(GameEvent.SaveGame, SceneEvent.JumpTo(MainMenuScene.name)))

      // // Quit
      // case KeyboardEvent.KeyUp(Key.KEY_3) =>
      //   Outcome(model, Batch(SceneEvent.JumpTo(MainMenuScene.name)))

      case e =>
        model.components.update(context)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UiContext[GameWindowContext],
        model: DropMenu
    ): Outcome[Layer] =
      model.components.present(context).map(_.toLayer)

    def cascade(model: DropMenu, newBounds: Bounds): DropMenu =
      model.copy(components = model.components.cascade(newBounds))

    def refresh(model: DropMenu): DropMenu =
      model.copy(components = model.components.reflow)
