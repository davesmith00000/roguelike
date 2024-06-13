package roguelike.windows

import indigo.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.GameWindowContext
import roguelike.model.Message
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*

object UnequipWindow:

  val windowId: WindowId = WindowId("Unequip")

  private val windowSize = Dimensions(35, 22)

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[UnequipMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      UnequipMenu.initial(charSheet)
    )
      .withTitle("Unequip Item")
      .moveTo(((screenSize - windowSize) / 2).toCoords)
      .resizeTo(windowSize)

final case class UnequipMenu(components: ComponentList[GameWindowContext])
object UnequipMenu:

  def initial(charSheet: CharSheet): UnequipMenu =
    UnequipMenu(
      ComponentList(Dimensions(35, 22)) { (_: GameWindowContext) =>
        Batch(
          TextArea[GameWindowContext](
            "Select a item to unequip\n ",
            TextArea.Theme(charSheet)
          )
        )
      }
        .withLayout(ComponentLayout.Vertical(Padding.zero))
        .add((ctx: GameWindowContext) =>
          // val collectables: Batch[(String, Int)] =
          //   ctx.player.inventory.backpack.items
          //     .map(_.name)
          //     .zipWithIndex
          //     .map { case (name, num) =>
          //       s"$num. $name" -> num
          //     }
          val collectables: Batch[(String, Option[Int])] =
            Batch(
              ctx.player.inventory.equipment.armour
                .map(a => "[0] " + a.name -> Option(0))
                .getOrElse("[ ] --No armour equipped--" -> None),
              ctx.player.inventory.equipment.weapon
                .map(m => "[1] " + m.name -> Option(1))
                .getOrElse("[ ] --No melee weapon equipped--" -> None)
            )

          collectables.map { (label, num) =>
            Button[GameWindowContext](label, Button.Theme(charSheet))
              .onClick(
                num match
                  case None =>
                    Batch(
                      GameEvent.Log(Message("Cannot unequip empty slot.", ColorScheme.invalid)),
                      GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                    )

                  case Some(0) =>
                    Batch(
                      GameEvent.Inventory(InventoryEvent.UnequipArmour),
                      GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                    )

                  case Some(1) =>
                    Batch(
                      GameEvent.Inventory(InventoryEvent.UnequipWeapon),
                      GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                    )

                  case Some(_) =>
                    Batch(
                      GameEvent.Log(Message("Invalid unequip slot.", ColorScheme.invalid)),
                      GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
                    )
              )
          }
        )
    )

  given WindowContent[UnequipMenu, GameWindowContext] with

    // This function takes a key, uses the code to check its a numeric value, and then return that value as an Option
    private def keyToNumber(keyCode: Int): Option[Int] =
      if keyCode >= Key.KEY_0.code && keyCode <= Key.KEY_9.code then
        Option(keyCode - Key.KEY_0.code)
      else None

    def updateModel(
        context: UIContext[GameWindowContext],
        model: UnequipMenu
    ): GlobalEvent => Outcome[UnequipMenu] =
      case KeyboardEvent.KeyUp(key) =>
        keyToNumber(key.code) match
          case None =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Log(Message("Cannot unequip empty slot.", ColorScheme.invalid)),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

          case Some(0) =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Inventory(InventoryEvent.UnequipArmour),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

          case Some(1) =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Inventory(InventoryEvent.UnequipWeapon),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

          case Some(_) =>
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Log(Message("Invalid unequip slot.", ColorScheme.invalid)),
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

      case e =>
        model.components.update(context)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UIContext[GameWindowContext],
        model: UnequipMenu
    ): Outcome[Layer] =
      model.components.present(context).map(_.toLayer)

    def refresh(
        reference: GameWindowContext,
        model: UnequipMenu,
        contentDimensions: Dimensions
    ): UnequipMenu =
      model.copy(components = model.components.refresh(reference, contentDimensions))
