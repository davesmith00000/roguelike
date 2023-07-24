package dungeongen.js

import roguelike.model.dungeon.Dungeon
import roguelike.model.dungeon.DungeonMetadata
import roguelike.model.dungeon.PositionedTile

import scala.scalajs.js

import js.JSConverters.*

trait JsDungeon extends js.Object:
  val playerStart: JsPoint
  val stairsPosition: JsPoint
  val positionedTiles: js.Array[JsPointGameTileTuple]
  val hostiles: js.Array[JsHostile]
  val collectables: js.Array[JsCollectable]
  val currentFloor: Int
  val meta: js.Array[JsPoint]

object JsDungeon:
  def fromDungeon(d: Dungeon) =
    new JsDungeon {
      val playerStart: JsPoint    = JsPoint.fromPoint(d.playerStart)
      val stairsPosition: JsPoint = JsPoint.fromPoint(d.stairsPosition)
      val positionedTiles: js.Array[JsPointGameTileTuple] =
        d.positionedTiles.map(JsPointGameTileTuple.fromPositionedTile).toJSArray
      val hostiles: js.Array[JsHostile] =
        d.hostiles.map(JsHostile.fromHostile(_)).toJSArray
      val collectables: js.Array[JsCollectable] = d.collectables.map {
        JsCollectable.fromCollectable(_)
      }.toJSArray
      val currentFloor: Int = d.currentFloor
      val meta: js.Array[JsPoint] = d.meta.roomCenters.map(JsPoint.fromPoint).toJSArray
    }

  def toDungeon(d: JsDungeon) =
    Dungeon(
      JsPoint.toPoint(d.playerStart),
      JsPoint.toPoint(d.stairsPosition),
      d.positionedTiles.map(JsPointGameTileTuple.toPositionedTile).toList,
      d.hostiles.map {
        JsHostile.toHostile(_)
      }.toList,
      d.collectables.map {
        JsCollectable.toCollectable(_)
      }.toList,
      d.currentFloor,
      DungeonMetadata(d.meta.toList.map(JsPoint.toPoint))
    )
