package roguelike.model.entity

import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

object ItemHelper:

  def findByName(name: String): Option[Item] =
    Armour.values
      .find(_.name == name)
      .orElse(Consumables.values.find(_.name == name))
      .orElse(Ranged.values.find(_.name == name))
      .orElse(Melee.values.find(_.name == name))

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def findByNameOrError(name: String): Item =
    findByName(name)
      .getOrElse {
        throw new Exception(s"Error, unknown Item '$name'.")
      }

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def findMeleeByNameOrError(name: String): Melee =
    Melee.values.find(_.name == name)
      .getOrElse {
        throw new Exception(s"Error, unknown Melee '$name'.")
      }

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def findArmourByNameOrError(name: String): Armour =
    Armour.values.find(_.name == name)
      .getOrElse {
        throw new Exception(s"Error, unknown Armour '$name'.")
      }
