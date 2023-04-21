package roguelike.model.js

import indigo.shared.datatypes.Point
import roguelike.model.GameTile
import roguelike.model.PositionedTile

import scala.scalajs.js

trait JsPointGameTileTuple extends js.Object:
  val point: JsPoint
  val gameTile: JsGameTile

object JsPointGameTileTuple:
  def fromPositionedTile(t: PositionedTile): JsPointGameTileTuple =
    new JsPointGameTileTuple {
      val point: JsPoint       = JsPoint.fromPoint(t.position)
      val gameTile: JsGameTile = JsGameTile.fromGameTile(t.tile)
    }

  def toPositionedTile(t: JsPointGameTileTuple): PositionedTile =
    PositionedTile(
      JsPoint.toPoint(t.point),
      JsGameTile.toGameTile(t.gameTile)
    )
