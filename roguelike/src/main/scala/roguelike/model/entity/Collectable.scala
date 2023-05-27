package roguelike.model.entity

import indigo.*
import io.circe.*
import io.circe.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.model.Equipment
import roguelike.model.GameMap
import roguelike.model.Inventory
import roguelike.model.Message
import roguelike.model.SharedCodecs
import roguelike.model.items.Item
// import roguelike.model.entity.Weapon

final case class Collectable(position: Point, item: Item) extends Entity:
  val blocksMovement: Boolean = false
  def name: String            = item.name

  def moveTo(newPosition: Point): Collectable =
    this.copy(position = newPosition)

object Collectable:

  import SharedCodecs.given

  given Encoder[Collectable] = new Encoder[Collectable] {
    final def apply(data: Collectable): Json = Json.obj(
      // ("position", data.position.asJson),
      // ("item", data.item.asJson)
    )
  }

  given Decoder[Collectable] = new Decoder[Collectable] {
    final def apply(c: HCursor): Decoder.Result[Collectable] =
      ???
    // for {
    //   position   <- c.downField("position").as[Point]
    //   item <- c.downField("item").as[Item]
    // } yield Collectable(position, item)
  }
