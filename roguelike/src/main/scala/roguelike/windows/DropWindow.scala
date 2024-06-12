package roguelike.windows

import indigo.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.GameWindowContext
import roguelike.model.Message
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

final case class DropMenu(components: ComponentList[GameWindowContext])
object DropMenu:

  def initial(charSheet: CharSheet): DropMenu =
    DropMenu(
      ComponentList(Dimensions(35, 22)) { (_: GameWindowContext) =>
        Batch(
          Label[GameWindowContext]("Select a collectable to drop", Label.Theme(charSheet))
        )
      }
        .withLayout(ComponentLayout.Vertical(Padding.zero.withBottom(1)))
        .add(ctx =>
          if ctx.player.inventory.backpack.count > 0 then Batch.empty
          else
            Batch(
              Label(
                "(Your inventory is empty.)",
                Label.Theme(charSheet)
              )
            )
        )
        .add((ctx: GameWindowContext) =>
          if ctx.player.inventory.backpack.count > 0 then
            val collectables: Batch[String] =
              ctx.player.inventory.backpack.items
                .map(_.name)
                .zip(UIElements.letters)
                .map { case (name, letter) =>
                  s"[$letter] $name"
                }

            collectables.map { label =>
              Button[GameWindowContext](label, Button.Theme(charSheet))
            // TODO: .onClick(GameEvent.Inventory(InventoryEvent.DropItem))
            // Also there is a bug where opening the drop window with the keyboard drops entry 'd' from the inventory
            }
          else Batch.empty
        )
    )

  given WindowContent[DropMenu, GameWindowContext] with

    def updateModel(
        context: UIContext[GameWindowContext],
        model: DropMenu
    ): GlobalEvent => Outcome[DropMenu] =
      case KeyboardEvent.KeyUp(key) =>
        UIElements.letterPositions.get(key.key) match
          case None =>
            Outcome(model)

          case Some(_) if context.reference.standingOn.isDefined =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Log(Message("Cannot drop here.", ColorScheme.invalid)),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

          case Some(keyIndex) =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Inventory(InventoryEvent.DropItem(keyIndex)),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

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
