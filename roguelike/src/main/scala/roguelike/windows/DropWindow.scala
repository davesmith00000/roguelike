package roguelike.windows

import indigo.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.GameWindowContext
import roguelike.model.Message
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
          TextArea[GameWindowContext](
            "Select a collectable to drop\n ",
            TextArea.Theme(charSheet)
          )
        )
      }
        .withLayout(ComponentLayout.Vertical(Padding.zero))
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
            val collectables: Batch[(String, Int)] =
              ctx.player.inventory.backpack.items
                .map(_.name)
                .zipWithIndex
                .map { case (name, num) =>
                  s"$num. $name" -> num
                }

            collectables.map { (label, num) =>
              Button[GameWindowContext](label, Button.Theme(charSheet))
                .onClick(
                  if ctx.standingOn.isDefined then
                    Batch(
                      GameEvent.Log(Message("Cannot drop here.", ColorScheme.invalid)),
                      GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                    )
                  else
                    Batch(
                      GameEvent.Inventory(InventoryEvent.DropItem(num)),
                      GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                    )
                )
            // Also there is a bug where opening the drop window with the keyboard drops entry 'd' from the inventory
            }
          else Batch.empty
        )
    )

  given WindowContent[DropMenu, GameWindowContext] with

    // This function takes a key, uses the code to check its a numeric value, and then return that value as an Option
    private def keyToNumber(keyCode: Int): Option[Int] =
      if keyCode >= Key.KEY_0.code && keyCode <= Key.KEY_9.code then
        Option(keyCode - Key.KEY_0.code)
      else None

    def updateModel(
        context: UIContext[GameWindowContext],
        model: DropMenu
    ): GlobalEvent => Outcome[DropMenu] =
      case KeyboardEvent.KeyUp(key) =>
        keyToNumber(key.code) match
          case None =>
            Outcome(model)

          case Some(_) if context.reference.standingOn.isDefined =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Log(Message("Cannot drop here.", ColorScheme.invalid)),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

          case Some(num) =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Inventory(InventoryEvent.DropItem(num)),
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
