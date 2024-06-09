package roguelike.windows

import indigo.*
import roguelike.model.GameWindowContext
import roguelike.scenes.UIElements
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*

object DropWindow:

  val windowId: WindowId = WindowId("Drop")

  private val windowSize = Dimensions(35, 22)

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
      ComponentGroup()
        .withLayout(ComponentLayout.Vertical())
        .add(
          TextArea(
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

                "Select a collectable to drop\n\n" + collectables
              else s"""Select an collectable to drop
                |
                |(Empty)
                |""".stripMargin,
            TextArea.Theme(charSheet)
          )
        )
    )

  given WindowContent[DropMenu, GameWindowContext] with

    def updateModel(
        context: UIContext[GameWindowContext],
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
        context: UIContext[GameWindowContext],
        model: DropMenu
    ): Outcome[Layer] =
      model.components.present(context).map(_.toLayer)

    def refresh(
        reference: GameWindowContext,
        model: DropMenu,
        contentDimensions: Dimensions
    ): DropMenu =
      model.copy(components = model.components.refresh(reference, contentDimensions))
