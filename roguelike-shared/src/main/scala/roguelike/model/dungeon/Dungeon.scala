package roguelike.model.dungeon

import indigo._
import roguelike.model.entity._

final case class Dungeon(
    playerStart: Point,
    stairsPosition: Point,
    positionedTiles: List[PositionedTile],
    hostiles: List[Hostile],
    collectables: List[Collectable],
    currentFloor: Int,
    meta: DungeonMetadata
)

final case class DungeonMetadata(rooms: List[Rectangle])
