package dungeongen.js

import indigo.shared.datatypes.Rectangle

import scala.scalajs.js

trait JsRectangle extends js.Object:
  val position: JsPoint
  val size: JsSize

object JsRectangle:
  def fromRectangle(p: Rectangle) =
    new JsRectangle {
      val position: JsPoint = JsPoint.fromPoint(p.position)
      val size: JsSize      = JsSize.fromSize(p.size)
    }

  def toRectangle(p: JsRectangle) = Rectangle(JsPoint.toPoint(p.position), JsSize.toSize(p.size))
