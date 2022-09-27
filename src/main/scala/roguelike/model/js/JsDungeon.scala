package roguelike.model.js

import roguelike.model.Dungeon

import scala.scalajs.js

import js.JSConverters.*

trait JsDungeon extends js.Object:
  val playerStart: JsPoint
  val stairsPosition: JsPoint
  val positionedTiles: js.Array[JsPointGameTileTuple]
  val hostiles: js.Array[JsHostile]
  val collectables: js.Array[JsCollectable]
  val currentFloor: Int

object JsDungeon:
  def fromDungeon(d: Dungeon) =
    new JsDungeon {
      val playerStart: JsPoint    = JsPoint.fromPoint(d.playerStart)
      val stairsPosition: JsPoint = JsPoint.fromPoint(d.stairsPosition)
      val positionedTiles: js.Array[JsPointGameTileTuple] =
        d.positionedTiles.map {
          JsPointGameTileTuple.fromTuple(_)
        }.toJSArray
      val hostiles: js.Array[JsHostile] =
        d.hostiles.map(JsHostile.fromHostile(_)).toJSArray
      val collectables: js.Array[JsCollectable] = d.collectables.map {
        JsCollectable.fromCollectable(_)
      }.toJSArray
      val currentFloor: Int = d.currentFloor
    }

  def toDungeon(d: JsDungeon) =
    Dungeon(
      JsPoint.toPoint(d.playerStart),
      JsPoint.toPoint(d.stairsPosition),
      d.positionedTiles.map {
        JsPointGameTileTuple.toTuple(_)
      }.toList,
      d.hostiles.map {
        JsHostile.toHostile(_)
      }.toList,
      d.collectables.map {
        JsCollectable.toCollectable(_)
      }.toList,
      d.currentFloor
    )
