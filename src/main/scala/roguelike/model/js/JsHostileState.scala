package roguelike.model.js

import roguelike.model.entity.HostileState

import scala.scalajs.js

trait JsHostileState extends js.Object:
  val name: String
  val remaining: Int

object JsHostileState:
  def fromHostileState(hs: HostileState) =
    hs match {
      case HostileState.Normal =>
        new JsHostileState {
          val name: String   = "normal"
          val remaining: Int = 0
        }
      case HostileState.Confused(r) =>
        new JsHostileState {
          val name: String   = "confused"
          val remaining: Int = r
        }
    }
  def toHostileState(hs: JsHostileState) =
    hs.name match {
      case "confused" => HostileState.Confused(hs.remaining)
      case _          => HostileState.Normal
    }
