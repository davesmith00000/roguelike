package roguelike.model.js

import indigo.shared.collections.Batch
import roguelike.model.GameMap

import scala.scalajs.js
import js.JSConverters._

trait JsGameMap extends js.Object:
  val size: JsSize
  val tileMap: js.Array[Option[JsGameTile]]
  val visible: js.Array[JsPoint]
  val explored: js.Array[JsPoint]

object JsGameMap:
  def fromGameMap(gm: GameMap) =
    new JsGameMap {
      val size: JsSize = JsSize.fromSize(gm.size)
      val tileMap: js.Array[Option[JsGameTile]] =
        gm.tileMap.map {
          _.map(gt => JsGameTile.fromGameTile(gt))
        }.toJSArray
      val visible: js.Array[JsPoint] = gm.visible.map {
        JsPoint.fromPoint(_)
      }.toJSArray
      val explored: js.Array[JsPoint] = gm.explored.map {
        JsPoint.fromPoint(_)
      }.toJSArray
    }

  def toGameMap(gm: JsGameMap) =
    GameMap(
      JsSize.toSize(gm.size),
      Batch.fromJSArray(gm.tileMap.map {
        _.map(gt => JsGameTile.toGameTile(gt))
      }),
      Batch.fromJSArray(gm.visible.map {
        JsPoint.toPoint(_)
      }),
      gm.explored.map {
        JsPoint.toPoint(_)
      }.toSet
    )
