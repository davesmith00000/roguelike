package roguelike.model

import indigo.*
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged

class InventoryTests extends munit.FunSuite {

  val inv =
    Inventory(
      Backpack(
        10,
        Batch(
          Consumables.HealthPotion,
          Ranged.LightningScroll,
          Ranged.FireballScroll,
          Melee.Dagger
        )
      ),
      Equipment(
        Option(Melee.Sword),
        Option(Armour.LeatherArmor)
      )
    )

  test("show Item") {
    assert(inv.item(1) == Some(Ranged.LightningScroll))
  }

  test("show Melee") {
    assert(inv.equippedWeapon == Some(Melee.Sword))
  }

  test("show Armour") {
    assert(inv.equippedArmour == Some(Armour.LeatherArmor))
  }

  test("add") {
    val actual = inv.add(Consumables.HealthPotion)

    assert(actual.backpack.count == 5)
    assert(actual.backpack.isFull == false)
    assert(actual.item(4) == Some(Consumables.HealthPotion))
  }

  test("remove") {
    val actual = inv.remove(1)

    assert(actual.backpack.count == 3)
    assert(actual.backpack.isFull == false)
    assert(actual.item(1) == Some(Ranged.FireballScroll))
  }

  test("extract") {
    val (maybeItem, nextInv) = inv.extract(1)

    assert(maybeItem == Some(Ranged.LightningScroll))
    assert(nextInv.backpack.count == 3)
    assert(nextInv.backpack.isFull == false)
    assert(nextInv.item(1) == Some(Ranged.FireballScroll))
  }

  test("use - consumable") {
    val next = inv.use(0)

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(actual.backpack.count == 3)
    assert(actual.backpack.isFull == false)
    assert(actual.item(0) == Some(Ranged.LightningScroll))
    assert(
      events.head == GameEvent.Inventory(
        InventoryEvent.UseConsumables(Consumables.HealthPotion)
      )
    )
  }

  test("use - scroll") {
    // This is odd, because the scroll isn't consumed yet,
    // in case there is no target available...
    val next = inv.use(1)

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(actual.backpack.count == 4)
    assert(actual.backpack.isFull == false)
    assert(actual.item(1) == Some(Ranged.LightningScroll))
    assert(
      events.head == GameEvent.Inventory(
        InventoryEvent.UseRanged(1, Ranged.LightningScroll)
      )
    )
  }

  test("use - weapon") {
    val next = inv.use(3)

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(actual.backpack.count == 4)
    assert(actual.backpack.isFull == false)
    assert(actual.item(3) == Some(Melee.Sword))
    assert(events.reverse.head == GameEvent.PlayerTurnEnd)
  }

  test("drop") {
    val next = inv.drop(1, Point.zero)

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(actual.backpack.count == 3)
    assert(actual.backpack.isFull == false)
    assert(actual.item(1) == Some(Ranged.FireballScroll))
    assert(
      events.head == GameEvent.Inventory(
        InventoryEvent.DropItem(Ranged.LightningScroll, Point.zero)
      )
    )
    assert(events.reverse.head == GameEvent.PlayerTurnEnd)
  }

  test("equip armour") {
    val next =
      inv.equipArmour(Armour.ChainMail)

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(clue(actual.last) == Some(Armour.LeatherArmor))
    assert(clue(actual.equippedArmour) == Some(Armour.ChainMail))
    assert(events.reverse.head == GameEvent.PlayerTurnEnd)
  }

  test("equip weapon") {
    val next =
      inv.equipWeapon(Melee.Dagger)

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(clue(actual.last) == Some(Melee.Sword))
    assert(clue(actual.equippedWeapon) == Some(Melee.Dagger))
    assert(events.reverse.head == GameEvent.PlayerTurnEnd)
  }

  test("unequip armour") {
    val next =
      inv.unequipArmour

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(clue(actual.last) == Some(Armour.LeatherArmor))
    assert(clue(actual.equippedArmour) == None)
    assert(events.reverse.head == GameEvent.PlayerTurnEnd)
  }

  test("unequip weapon") {
    val next =
      inv.unequipWeapon

    val actual = next.unsafeGet
    val events = next.unsafeGlobalEvents

    assert(clue(actual.last) == Some(Melee.Sword))
    assert(clue(actual.equippedWeapon) == None)
    assert(events.reverse.head == GameEvent.PlayerTurnEnd)
  }

}
