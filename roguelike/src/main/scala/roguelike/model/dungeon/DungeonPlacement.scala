package roguelike.model.dungeon

import indigo.*
import roguelike.model.entity.*
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

object DungeonPlacement:

  def placeEntities(
      floor: Int,
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxMonstersPerRoom: Int
  ): List[Hostile] =
    DungeonRules
      .randomChoices(
        dice,
        maxMonstersPerRoom,
        floor,
        DungeonRules.enemyChances
      )
      .zipWithIndex
      .flatMap {
        case (Orc.name, i) =>
          val x = dice.rollFromZero(room.width - 4) + room.left + 2
          val y = dice.rollFromZero(room.height - 4) + room.top + 2

          List(Orc.spawn(entityCount + i, Point(x, y)))

        case (Troll.name, i) =>
          val x = dice.rollFromZero(room.width - 4) + room.left + 2
          val y = dice.rollFromZero(room.height - 4) + room.top + 2

          List(Troll.spawn(entityCount + i, Point(x, y)))

        case _ =>
          Nil
      }
      .distinct

  def placeCollectables(
      floor: Int,
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxCollectablesPerRoom: Int,
      hostiles: List[Hostile],
      roomCenter: Point
  ): List[Collectable] =
    def spawn(item: Item): List[Collectable] =
      val x   = dice.rollFromZero(room.width - 4) + room.left + 2
      val y   = dice.rollFromZero(room.height - 4) + room.top + 2
      val pos = Point(x, y)

      if hostiles.contains(pos) || pos == roomCenter then Nil
      else List(Collectable(pos, item))

    DungeonRules
      .randomChoices(
        dice,
        maxCollectablesPerRoom,
        floor,
        DungeonRules.collectableChances
      )
      .flatMap {
        case Consumables.HealthPotion.name =>
          spawn(Consumables.HealthPotion)

        case Ranged.FireballScroll.name =>
          spawn(Ranged.FireballScroll)

        case Ranged.ConfusionScroll.name =>
          spawn(Ranged.ConfusionScroll)

        case Ranged.LightningScroll.name =>
          spawn(Ranged.LightningScroll)

        case Melee.Dagger.name =>
          spawn(Melee.Dagger)

        case Melee.Sword.name =>
          spawn(Melee.Sword)

        case Armour.LeatherArmor.name =>
          spawn(Armour.LeatherArmor)

        case Armour.ChainMail.name =>
          spawn(Armour.ChainMail)

        case _ =>
          Nil
      }
      .distinct
