package roguelike

import indigo.Point
import indigo.shared.events.GlobalEvent
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

  // Inventory events
  case Inventory(event: InventoryEvent)

  // Phase management
  case NPCTurnComplete

  // Hostile events
  case HostileMeleeAttack(attackerName: String, power: Int)
  case HostileGiveXP(amount: Int)

  // System events
  case Log(message: Message)
  case Redraw
  case RedrawHistoryLog
  case Targeted(position: Point)

  // ViewModel events
  case CameraSnapToPlayer
  case ViewModelHandOff(event: ViewModelEvent)
  case ModelHandOff(event: ViewModelEvent)
  case ViewModelPhaseComplete(event: ViewModelEvent)

enum InventoryEvent:
  case UseConsumables(consumables: Consumables)
  case ReturnConsumablesToInventory(consumables: Consumables)

  case EquipArmour(armour: Armour)
  case UnequipArmour
  case ReturnArmourToInventory(armour: Armour)
  case EquipMelee(melee: Melee)
  case UnequipMelee
  case ReturnMeleeToInventory(melee: Melee)
  case DropItem(item: Item, mapPosition: Point)
  case UseRanged(inventoryPosition: Int, ranged: Ranged)
  case TargetUsingRanged(inventoryPosition: Int, ranged: Ranged)
  case PickedUp(item: Item)

enum ViewModelEvent:
  case MovePlayer
  case PlayerMoveComplete
