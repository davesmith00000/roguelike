package dungeongen.js

import roguelike.model.GameTile
import roguelike.model.dungeon.WallCode

import scala.scalajs.js

trait JsGameTile extends js.Object:
  val name: String

object JsGameTile:
  def fromGameTile(gt: GameTile) =
    new JsGameTile {
      val name: String = gt match {
        case GameTile.Ground(style) => "g" + style.toString
        case GameTile.DownStairs    => "d"
        case GameTile.Wall(code)    => "w" + code.toCode
      }
    }

  def toGameTile(gt: JsGameTile) = gt.name match {
    case s if s.startsWith("g") =>
      GameTile.Ground(s.drop(1).toInt)

    case s if s.startsWith("w") =>
      val c = s.drop(1)
      GameTile.Wall(if c.isEmpty then WallCode.Wall else WallCode.fromCode(c))

    case "d" =>
      GameTile.DownStairs

    case _ =>
      GameTile.Wall(WallCode.Wall)
  }
