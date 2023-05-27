package dungeongen.js

import indigo.shared.datatypes.Point

import scala.scalajs.js

trait JsPoint extends js.Object:
  val x: Int
  val y: Int

object JsPoint:
  def fromPoint(p: Point) =
    new JsPoint {
      val x: Int = p.x
      val y: Int = p.y
    }

  def toPoint(p: JsPoint) = Point(p.x, p.y)
