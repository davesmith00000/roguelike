package roguelike.model

import indigo.Dice
import indigo.Outcome
import indigo.RGB
import io.circe._
import io.circe.syntax._
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Melee
import roguelike.model.items.Item

final case class Equipment(weapon: Option[Melee], armour: Option[Armour]):

  def defenseBonus: Int =
    armour.map(_.defenseBonus).getOrElse(0)

  def powerBonus: Int =
    weapon.map(_.power).getOrElse(0)

  def equip(equipment: Melee | Armour): Outcome[Equipment] =
    equipment match
      case w: Melee =>
        Outcome(this.copy(weapon = Option(w)))
          .addGlobalEvents(
            GameEvent.Log(Message(s"You equip the ${w.name}.", RGB.White)) ::
              weapon
                .map(m =>
                  GameEvent.Inventory(InventoryEvent.ReturnMeleeToInventory(m))
                )
                .toList
          )

      case a: Armour =>
        Outcome(this.copy(armour = Option(a)))
          .addGlobalEvents(
            GameEvent.Log(Message(s"You equip the ${a.name}.", RGB.White)) ::
              armour
                .map(a =>
                  GameEvent.Inventory(InventoryEvent.ReturnArmourToInventory(a))
                )
                .toList
          )

  def unequipArmour: Outcome[Equipment] =
    armour match
      case None =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"No armour to unequip.", ColorScheme.invalid)
            )
          )

      case Some(a) =>
        Outcome(this.copy(armour = None))
          .addGlobalEvents(
            GameEvent.Inventory(InventoryEvent.ReturnArmourToInventory(a)),
            GameEvent.Log(Message(s"You remove the ${a.name}.", RGB.White))
          )

  def unequipWeapon: Outcome[Equipment] =
    weapon match
      case None =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"No melee weapon to unequip.", ColorScheme.invalid)
            )
          )

      case Some(w) =>
        Outcome(this.copy(weapon = None))
          .addGlobalEvents(
            GameEvent.Inventory(InventoryEvent.ReturnMeleeToInventory(w)),
            GameEvent.Log(Message(s"You remove the ${w.name}.", RGB.White))
          )

object Equipment:

  val initial: Equipment =
    Equipment(
      Some(Melee.Dagger),
      Some(Armour.LeatherArmor)
    )

  given Encoder[Equipment] = new Encoder[Equipment] {
    final def apply(data: Equipment): Json = Json.obj(
      // ("weapon", data.weapon.asJson),
      // ("armour", data.armour.asJson)
    )
  }

  given Decoder[Equipment] = new Decoder[Equipment] {
    final def apply(c: HCursor): Decoder.Result[Equipment] =
      ???
    // for {
    //   // weapon <- c.downField("weapon").as[Option[Melee]]
    //   // armour <- c.downField("armour").as[Option[Armour]]
    // } yield Equipment(weapon, armour)
  }
