package roguelike.model.dungeon

import indigo.*
import roguelike.model.entity.*

final case class Dungeon(
    playerStart: Point,
    stairsPosition: Point,
    positionedTiles: List[PositionedTile],
    hostiles: List[Hostile],
    collectables: List[Collectable],
    currentFloor: Int
)
