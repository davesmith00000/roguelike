package roguelike.model.dungeon

import indigo._
import roguelike.model.GameTile

final case class PositionedTile(position: Point, tile: GameTile)
