package roguelike.model.js

import roguelike.model.GameTile

import scala.scalajs.js

trait JsGameTile extends js.Object:
  val name: String

object JsGameTile:
  def fromGameTile(gt: GameTile) =
    new JsGameTile {
      val name: String = gt match {
        case GameTile.Ground(style)    => "g" + style.toString
        case GameTile.DownStairs       => "d"
        case GameTile.Wall(Some(code)) => "w" + code
        case GameTile.Wall(None)       => "w"
      }
    }

  def toGameTile(gt: JsGameTile) = gt.name match {
    case s if s.startsWith("g") =>
      GameTile.Ground(s.drop(1).toInt)

    case s if s.startsWith("w") =>
      val c = s.drop(1)
      GameTile.Wall(if c.isEmpty then None else Some(c))

    case "d" =>
      GameTile.DownStairs

    case _ =>
      GameTile.Wall(None)
  }
