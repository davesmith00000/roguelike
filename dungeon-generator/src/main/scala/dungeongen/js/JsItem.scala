package dungeongen.js

import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.*

import scala.scalajs.js

trait JsItem extends js.Object:
  val name: String

object JsItem:
  def fromItem(i: Item) = new JsItem {
    val name: String = i.name
  }

  def toItem(i: JsItem) =
    i.name match {
      case Armour.ChainMail.name         => Armour.ChainMail
      case Armour.LeatherArmor.name      => Armour.LeatherArmor
      case Consumables.HealthPotion.name => Consumables.HealthPotion
      case Melee.Dagger.name             => Melee.Dagger
      case Melee.Sword.name              => Melee.Sword
      case Ranged.ConfusionScroll.name   => Ranged.ConfusionScroll
      case Ranged.FireballScroll.name    => Ranged.FireballScroll
      case Ranged.LightningScroll.name   => Ranged.LightningScroll
      case _ =>
        new Item {
          val name: String = i.name
        }
    }
