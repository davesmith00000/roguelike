package roguelike.model.js

import roguelike.model.entity.Hostile
import roguelike.model.entity.Orc
import roguelike.model.entity.Troll

import scala.scalajs.js

import js.JSConverters._

trait JsHostile extends js.Object:
  val id: Int
  val name: String
  val position: JsPoint
  val isAlive: Boolean
  val fighter: JsFighter
  val movePath: js.Array[JsPoint]
  val state: JsHostileState

object JsHostile:
  def fromHostile(h: Hostile) =
    new JsHostile {
      val id: Int            = h.id
      val name: String       = h.name
      val position: JsPoint  = JsPoint.fromPoint(h.position)
      val isAlive: Boolean   = h.isAlive
      val fighter: JsFighter = JsFighter.fromFighter(h.fighter)
      val movePath: js.Array[JsPoint] = h.movePath.map {
        JsPoint.fromPoint(_)
      }.toJSArray
      val state: JsHostileState = JsHostileState.fromHostileState(h.state)
    }

  def toHostile(h: JsHostile) =
    val position = JsPoint.toPoint(h.position)
    val fighter  = JsFighter.toFighter(h.fighter)
    val state    = JsHostileState.toHostileState(h.state)
    val movePath = h.movePath.map(JsPoint.toPoint(_)).toList
    h.name match {
      case Orc.name =>
        Orc(h.id, position, h.isAlive, fighter, movePath, state)

      case Troll.name =>
        Troll(h.id, position, h.isAlive, fighter, movePath, state)
    }
