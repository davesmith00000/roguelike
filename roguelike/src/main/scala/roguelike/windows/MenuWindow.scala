package roguelike.windows

import indigo.*
import roguelike.GameEvent
import roguelike.model.GameWindowContext
import roguelikestarterkit.*

object MenuWindow:

  val windowId: WindowId = WindowId("Menu")

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[MainMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      MainMenu.initial(charSheet)
    )
      .moveTo(0, 0)
      .resizeTo(
        Dimensions(screenSize.width, 3)
      )
      .isStatic

final case class MainMenu(components: ComponentGroup)
object MainMenu:

  def initial(charSheet: CharSheet): MainMenu =
    MainMenu(
      ComponentGroup(Bounds(0, 0, 16, 1))
        .withLayout(ComponentLayout.Horizontal(Padding.right(1)))
        .add(
          Batch(
            "Character" -> Batch(),
            "Inventory" -> Batch(),
            "Drop"      -> Batch(),
            "Equip"     -> Batch(),
            "History"   -> Batch(GameEvent.WindowEvent(WindowManagerCommand.ShowHistory)),
            "Controls"  -> Batch(GameEvent.WindowEvent(WindowManagerCommand.ShowControls)),
            "Quit"      -> Batch(GameEvent.WindowEvent(WindowManagerCommand.ShowQuit))
          ).map { case (label, clickEvents) =>
            Button(
              label,
              Button.Theme(charSheet)
            ).onClick(clickEvents)
          }
        )
    )

  given WindowContent[MainMenu, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: MainMenu
    ): GlobalEvent => Outcome[MainMenu] =
      case e =>
        model.components.update(context)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UiContext[GameWindowContext],
        model: MainMenu
    ): Outcome[Layer] =
      model.components.present(context).map { c =>
        Layer(
          c.nodes
        ).addNodes(
          Text(
            s"My Generic Roguelike",
            RoguelikeTiles.Size10x10.Fonts.fontInfo.fontKey,
            TerminalMaterial(context.charSheet.assetName, RGBA.White, RGBA.Zero)
          ).moveTo(
            context.screenSpaceBounds.horizontalCenter,
            context.screenSpaceBounds.position.y
          )
        ).addCloneBlanks(c.cloneBlanks)
      }

    def cascade(model: MainMenu, newBounds: Bounds): MainMenu =
      model.copy(components = model.components.cascade(newBounds))
