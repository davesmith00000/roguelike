package roguelike.model.js

import indigo.shared.datatypes.Point
import roguelike.model.GameTile

import scala.scalajs.js

trait JsPointGameTileTuple extends js.Object:
  val point: JsPoint
  val gameTile: JsGameTile

object JsPointGameTileTuple:
  def fromTuple(t: (Point, GameTile)) = new JsPointGameTileTuple {
    val point: JsPoint       = JsPoint.fromPoint(t._1)
    val gameTile: JsGameTile = JsGameTile.fromGameTile(t._2)
  }

  def toTuple(t: JsPointGameTileTuple) = (
    JsPoint.toPoint(t.point),
    JsGameTile.toGameTile(t.gameTile)
  )
