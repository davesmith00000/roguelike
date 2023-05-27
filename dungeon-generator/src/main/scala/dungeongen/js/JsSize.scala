package dungeongen.js

import indigo.shared.datatypes.Size

import scala.scalajs.js

trait JsSize extends js.Object:
  val width: Int
  val height: Int

object JsSize:
  def fromSize(s: Size) =
    new JsSize {
      val width: Int  = s.width
      val height: Int = s.height
    }

  def toSize(s: JsSize) = Size(s.width, s.height)
