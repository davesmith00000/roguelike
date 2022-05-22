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

final case class Inventory(capacity: Int, items: Batch[Item]):

  val isFull: Boolean =
    items.length >= capacity

  def add(item: Item): Outcome[Inventory] =
    val newItems = items :+ item
    Outcome(this.copy(items = newItems))

  def remove(at: Int): Outcome[Inventory] =
    val (start, end) = items.splitAt(at)
    Outcome(this.copy(items = start ++ end.drop(1)))

  def use(at: Int): Outcome[Inventory] =
    items.lift(at) match
      case None =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid))
          )

      case Some(c: Consumables) =>
        remove(at).addGlobalEvents(
          GameEvent.Inventory(InventoryEvent.UseConsumables(c))
        )

      case Some(m: Melee) =>
        remove(at).addGlobalEvents(
          GameEvent.Inventory(InventoryEvent.EquipMelee(m))
        )

      case Some(a: Armour) =>
        remove(at).addGlobalEvents(
          GameEvent.Inventory(InventoryEvent.EquipArmour(a))
        )

      case Some(Ranged.LightningScroll) =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Inventory(
              InventoryEvent.UseRanged(at, Ranged.LightningScroll)
            )
          )

      case Some(r: Ranged) =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Inventory(InventoryEvent.TargetUsingRanged(at, r))
          )

      case Some(i) =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Log(
              Message("Unhandled item: " + i.name, ColorScheme.invalid)
            )
          )

  def drop(at: Int, mapPosition: Point): Outcome[Inventory] =
    items.lift(at) match
      case None =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.Log(Message("Invalid entry.", ColorScheme.invalid))
          )

      case Some(item) =>
        val (start, end) = items.splitAt(at)
        Outcome(
          this.copy(
            items = start ++ end.drop(1)
          )
        ).addGlobalEvents(
          GameEvent.Inventory(InventoryEvent.DropItem(item, mapPosition)),
          GameEvent.Log(
            Message(
              s"You dropped the ${item.name}.",
              RGB.White
            )
          ),
          GameEvent.PlayerTurnEnd
        )

object Inventory:

  val initial: Inventory =
    Inventory(
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

  given Encoder[Inventory] = new Encoder[Inventory] {
    final def apply(data: Inventory): Json = Json.obj(
      // ("capacity", Json.fromInt(data.capacity)),
      // ("items", data.items.asJson)
    )
  }

  given Decoder[Inventory] = new Decoder[Inventory] {
    final def apply(c: HCursor): Decoder.Result[Inventory] =
      ???
    // for {
    //   capacity <- c.downField("capacity").as[Int]
    //   items    <- c.downField("items").as[List[Item]]
    // } yield Inventory(capacity, items)
  }
