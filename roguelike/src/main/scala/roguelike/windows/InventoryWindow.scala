package roguelike.windows

import indigo.*
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.GameWindowContext
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*

object InventoryWindow:

  val windowId: WindowId = WindowId("InventoryMenu")

  private val windowSize = Dimensions(35, 22)

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[InventoryMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      InventoryMenu.initial(charSheet)
    )
      .withTitle("Inventory")
      .moveTo(((screenSize - windowSize) / 2).toCoords)
      .resizeTo(windowSize)

final case class InventoryMenu(components: ComponentList[GameWindowContext])
object InventoryMenu:

  def initial(charSheet: CharSheet): InventoryMenu =
    InventoryMenu(
      ComponentList(Dimensions(35, 22)) { (_: GameWindowContext) =>
        Batch(
          TextArea[GameWindowContext](
            "Select an item to use\n ",
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
                  s"[$num] $name" -> num
                }

            collectables.map { (label, num) =>
              Button[GameWindowContext](label, Button.Theme(charSheet))
                .onClick(
                  Batch(
                    GameEvent.Inventory(InventoryEvent.UseItem(num)),
                    GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                  )
                )
            }
          else Batch.empty
        )
    )

  given WindowContent[InventoryMenu, GameWindowContext] with

    // This function takes a key, uses the code to check its a numeric value, and then return that value as an Option
    private def keyToNumber(keyCode: Int): Option[Int] =
      if keyCode >= Key.KEY_0.code && keyCode <= Key.KEY_9.code then
        Option(keyCode - Key.KEY_0.code)
      else None

    def updateModel(
        context: UIContext[GameWindowContext],
        model: InventoryMenu
    ): GlobalEvent => Outcome[InventoryMenu] =
      case KeyboardEvent.KeyUp(key) =>
        keyToNumber(key.code) match
          case None =>
            Outcome(model)

          case Some(num) =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Inventory(InventoryEvent.UseItem(num)),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

      case e =>
        model.components.update(context)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UIContext[GameWindowContext],
        model: InventoryMenu
    ): Outcome[Layer] =
      model.components.present(context).map(_.toLayer)

    def refresh(
        reference: GameWindowContext,
        model: InventoryMenu,
        contentDimensions: Dimensions
    ): InventoryMenu =
      model.copy(components = model.components.refresh(reference, contentDimensions))
