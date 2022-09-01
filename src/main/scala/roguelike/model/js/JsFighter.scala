package roguelike.model.js

import roguelike.model.entity.Fighter

import scala.scalajs.js

trait JsFighter extends js.Object:
  val hp: Int
  val maxHp: Int
  val defense: Int
  val power: Int

object JsFighter:
  def fromFighter(f: Fighter) = new JsFighter {
    val hp: Int      = f.hp
    val maxHp: Int   = f.maxHp
    val defense: Int = f.defense
    val power: Int   = f.power
  }

  def toFighter(f: JsFighter) = Fighter(
    f.hp,
    f.maxHp,
    f.defense,
    f.power
  )
