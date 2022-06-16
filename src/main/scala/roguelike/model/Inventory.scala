package roguelike.model

import indigo.*
import indigo.syntax.*
import io.circe._
import io.circe.syntax._
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.entity._
import roguelike.model.gamedata._
import roguelike.model.items.Item

final case class Inventory(backpack: Backpack, equipment: Equipment):

  def add(item: Item): Inventory =
    this.copy(backpack = backpack.add(item))

  def remove(at: Int): Inventory =
    this.copy(backpack = backpack.remove(at))

  def first: Option[Item] =
    item(0)

  def last: Option[Item] =
    item(backpack.count - 1)

  def item(at: Int): Option[Item] =
    backpack.item(at)

  def extract(at: Int): (Option[Item], Inventory) =
    val (maybeItem, next) = backpack.extract(at)
    (maybeItem, this.copy(backpack = next))

  def equippedWeapon: Option[Melee] =
    equipment.weapon

  def equippedArmour: Option[Armour] =
    equipment.armour

  def equip(equipment: Melee | Armour): Outcome[Inventory] =
    equipment match
      case w: Melee  => equipWeapon(w)
      case a: Armour => equipArmour(a)

  def equipArmour(armour: Armour): Outcome[Inventory] =
    equipment.equipArmour(armour).map { case (a, e) =>
      this.copy(
        equipment = e,
        backpack = a.map(backpack.add).getOrElse(backpack)
      )
    }

  def equipWeapon(weapon: Melee): Outcome[Inventory] =
    equipment.equipWeapon(weapon).map { case (w, e) =>
      this.copy(
        equipment = e,
        backpack = w.map(backpack.add).getOrElse(backpack)
      )
    }

  def unequipArmour: Outcome[Inventory] =
    equipment.unequipArmour.map { case (a, e) =>
      this.copy(
        equipment = e,
        backpack = a.map(backpack.add).getOrElse(backpack)
      )
    }

  def unequipWeapon: Outcome[Inventory] =
    equipment.unequipWeapon.map { case (w, e) =>
      this.copy(
        equipment = e,
        backpack = w.map(backpack.add).getOrElse(backpack)
      )
    }

  def use(at: Int): Outcome[Inventory] =
    val (maybeItem, next) = extract(at)

    maybeItem match
      case None =>
        Outcome(
          next,
          Batch(
            GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid))
          )
        )

      case Some(c: Consumables) =>
        Outcome(
          next,
          Batch(
            GameEvent.Inventory(InventoryEvent.UseConsumables(c))
          )
        )

      case Some(m: Melee) =>
        next.equipWeapon(m)

      case Some(a: Armour) =>
        next.equipArmour(a)

      case Some(Ranged.LightningScroll) =>
        Outcome(
          this,
          Batch(
            GameEvent.Inventory(
              InventoryEvent.UseRanged(at, Ranged.LightningScroll)
            )
          )
        )

      case Some(r: Ranged) =>
        Outcome(
          this,
          Batch(
            GameEvent.Inventory(InventoryEvent.TargetUsingRanged(at, r))
          )
        )

      case Some(i) =>
        Outcome(
          next,
          Batch(
            GameEvent.Log(
              Message("Unhandled item: " + i.name, ColorScheme.invalid)
            )
          )
        )

  def drop(at: Int, mapPosition: Point): Outcome[Inventory] =
    val (maybeItem, next) = extract(at)

    maybeItem match
      case None =>
        Outcome(
          next,
          Batch(
            GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid))
          )
        )

      case Some(item) =>
        Outcome(
          next,
          Batch(
            GameEvent.Inventory(InventoryEvent.DropItem(item, mapPosition)),
            GameEvent.Log(
              Message(
                s"You dropped the ${item.name}.",
                ColorScheme.white
              )
            ),
            GameEvent.PlayerTurnEnd
          )
        )

object Inventory:
  val initial: Inventory =
    Inventory(Backpack.initial, Equipment.initial)

final case class Backpack(capacity: Int, items: Batch[Item]):

  lazy val count: Int =
    items.length

  lazy val isFull: Boolean =
    count >= capacity

  def add(item: Item): Backpack =
    val newItems = items :+ item
    this.copy(items = newItems)

  def remove(at: Int): Backpack =
    val (start, end) = items.splitAt(at)
    this.copy(items = start ++ end.drop(1))

  def extract(at: Int): (Option[Item], Backpack) =
    val (start, end) = items.splitAt(at)
    (end.headOption, this.copy(items = start ++ end.drop(1)))

  def item(at: Int): Option[Item] =
    items.lift(at)

object Backpack:

  val initial: Backpack =
    Backpack(
      10,
      Batch(
        Consumables.HealthPotion,
        Melee.Sword,
        Armour.ChainMail,
        Ranged.ConfusionScroll,
        Ranged.LightningScroll,
        Ranged.FireballScroll
      )
    )

  given Encoder[Backpack] = new Encoder[Backpack] {
    final def apply(data: Backpack): Json = Json.obj(
      // ("capacity", Json.fromInt(data.capacity)),
      // ("items", data.items.asJson)
    )
  }

  given Decoder[Backpack] = new Decoder[Backpack] {
    final def apply(c: HCursor): Decoder.Result[Backpack] =
      ???
    // for {
    //   capacity <- c.downField("capacity").as[Int]
    //   items    <- c.downField("items").as[List[Item]]
    // } yield Backpack(capacity, items)
  }

final case class Equipment(weapon: Option[Melee], armour: Option[Armour]):

  def defenseBonus: Int =
    armour.map(_.defenseBonus).getOrElse(0)

  def powerBonus: Int =
    weapon.map(_.power).getOrElse(0)

  def equip(equipment: Melee | Armour): Outcome[(Option[Item], Equipment)] =
    equipment match
      case w: Melee  => equipWeapon(w)
      case a: Armour => equipArmour(a)

  def equipArmour(a: Armour): Outcome[(Option[Armour], Equipment)] =
    armour match
      case None =>
        Outcome((armour, this.copy(armour = Option(a))))
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"You equip the ${a.name}.", ColorScheme.white)
            ),
            GameEvent.PlayerTurnEnd
          )

      case Some(old) =>
        Outcome((armour, this.copy(armour = Option(a))))
          .addGlobalEvents(
            GameEvent.Log(
              Message(
                s"Returned ${old.name} to your inventory",
                ColorScheme.white
              )
            ),
            GameEvent.Log(
              Message(s"You equip the ${a.name}.", ColorScheme.white)
            ),
            GameEvent.PlayerTurnEnd
          )

  def equipWeapon(w: Melee): Outcome[(Option[Melee], Equipment)] =
    weapon match
      case None =>
        Outcome((weapon, this.copy(weapon = Option(w))))
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"You equip the ${w.name}.", ColorScheme.white)
            ),
            GameEvent.PlayerTurnEnd
          )

      case Some(old) =>
        Outcome((weapon, this.copy(weapon = Option(w))))
          .addGlobalEvents(
            GameEvent.Log(
              Message(
                s"Returned ${old.name} to your inventory",
                ColorScheme.white
              )
            ),
            GameEvent.Log(
              Message(s"You equip the ${w.name}.", ColorScheme.white)
            ),
            GameEvent.PlayerTurnEnd
          )

  def unequipArmour: Outcome[(Option[Armour], Equipment)] =
    armour match
      case None =>
        Outcome((armour, this))
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"No armour to unequip.", ColorScheme.invalid)
            ),
            GameEvent.PlayerTurnEnd
          )

      case Some(old) =>
        Outcome((armour, this.copy(armour = None)))
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"You remove the ${old.name}.", ColorScheme.white)
            ),
            GameEvent.PlayerTurnEnd
          )

  def unequipWeapon: Outcome[(Option[Melee], Equipment)] =
    weapon match
      case None =>
        Outcome((weapon, this))
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"No melee weapon to unequip.", ColorScheme.invalid)
            ),
            GameEvent.PlayerTurnEnd
          )

      case Some(old) =>
        Outcome((weapon, this.copy(weapon = None)))
          .addGlobalEvents(
            GameEvent.Log(
              Message(s"You remove the ${old.name}.", ColorScheme.white)
            ),
            GameEvent.PlayerTurnEnd
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
