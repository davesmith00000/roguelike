package roguelike

import indigo.Point
import indigo.shared.events.GlobalEvent
import roguelike.components.windows.WindowManagerCommand
import roguelike.model.Message
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

enum GameEvent extends GlobalEvent:
  // Player events
  case PlayerAttack(attackerName: String, power: Int, id: Int)
  case PlayerCastsConfusion(
      attackerName: String,
      turns: Int,
      id: Int
  )
  case PlayerCastsFireball(
      attackerName: String,
      damage: Int,
      id: Int
  )
  case PlayerTurnEnd
  case PlayerMoveTowards(target: Point) // Move towards tile on click.
  case PlayerContinueMove               // Continue moving towards the target.
  case PlayerTryPickUp
  case PlayerMoveComplete
  case PlayerDescended

  // Inventory events
  case Inventory(event: InventoryEvent)

  // Phase management
  case NPCTurnComplete
  case NPCMoveComplete

  // Hostile events
  case Hostile(event: HostileEvent)

  // System events
  case Log(message: Message)
  case RedrawHistoryLog
  case Targeted(position: Point)
  case WindowEvent(command: WindowManagerCommand)

  // ViewModel events
  case CameraSnapToPlayer

enum HostileEvent:
  case HostileMeleeAttack(attackerName: String, power: Int)
  case HostileGiveXP(amount: Int)

enum InventoryEvent:
  case UseConsumables(consumables: Consumables)
  case DropItem(item: Item, mapPosition: Point)
  case UseRanged(inventoryPosition: Int, ranged: Ranged)
  case TargetUsingRanged(inventoryPosition: Int, ranged: Ranged)
  case PickedUp(item: Item)
  case RemoveFromInventory(inventoryPosition: Int)
