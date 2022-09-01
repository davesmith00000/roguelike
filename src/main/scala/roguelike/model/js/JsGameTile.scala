package roguelike.model.js

import roguelike.model.GameTile

import scala.scalajs.js

trait JsGameTile extends js.Object:
  val name: String

object JsGameTile:
  def fromGameTile(gt: GameTile) =
    new JsGameTile {
      val name: String = gt match {
        case GameTile.Ground     => "g"
        case GameTile.DownStairs => "d"
        case GameTile.Wall       => "w"
      }
    }

  def toGameTile(gt: JsGameTile) = gt.name match {
    case "g" => GameTile.Ground
    case "d" => GameTile.DownStairs
    case _   => GameTile.Wall
  }
