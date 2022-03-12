package roguelike.model

import indigo._
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged

class DungeonGenTests extends munit.FunSuite {

  test("random choices floor 0") {
    val chances =
      DungeonGen.collectableChances

    val count: Int = 50
    val floor: Int = 0

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(123), count, floor, chances)

    assert(actual.length == count)
    assert(actual.forall(_.name == Consumables.HealthPotion.name))
  }

  test("random choices floor 2") {
    val chances =
      DungeonGen.collectableChances

    val count: Int = 50
    val floor: Int = 2

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(456), count, floor, chances)

    val possibilities = List(
      Consumables.HealthPotion.name,
      Ranged.ConfusionScroll.name
    )

    assert(actual.length == count)
    assert(actual.forall(p => possibilities.contains(p)))
  }

  test("random choices floor 4") {
    val chances =
      DungeonGen.collectableChances

    val count: Int = 50
    val floor: Int = 4

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(789), count, floor, chances)

    val possibilities = List(
      Consumables.HealthPotion.name,
      Ranged.ConfusionScroll.name,
      Ranged.LightningScroll.name,
      Melee.Sword.name
    )

    assert(actual.length == count)
    assert(actual.forall(p => possibilities.contains(p)))
  }

  test("random choices floor 6") {
    val chances =
      DungeonGen.collectableChances

    val count: Int = 50
    val floor: Int = 6

    val actual =
      DungeonGen.randomChoices(Dice.fromSeed(6181151), count, floor, chances)

    val possibilities = List(
      Consumables.HealthPotion.name,
      Ranged.ConfusionScroll.name,
      Ranged.LightningScroll.name,
      Ranged.FireballScroll.name,
      Melee.Sword.name,
      Armour.ChainMail.name
    )

    assert(actual.length == count)
    assert(actual.forall(p => possibilities.contains(p)))
  }

}
