package dungeongen.js

import roguelike.model.entity.Collectable

import scala.scalajs.js

trait JsCollectable extends js.Object:
  val position: JsPoint
  val item: JsItem

object JsCollectable:
  def fromCollectable(c: Collectable) =
    new JsCollectable {
      val position: JsPoint = JsPoint.fromPoint(c.position)
      val item: JsItem      = JsItem.fromItem(c.item)
    }

  def toCollectable(c: JsCollectable) =
    Collectable(JsPoint.toPoint(c.position), JsItem.toItem(c.item))
