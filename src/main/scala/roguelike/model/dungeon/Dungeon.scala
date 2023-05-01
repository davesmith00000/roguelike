package roguelike.model.dungeon

import indigo.*
import roguelike.model.entity.*
// import roguelike.model.gamedata.Armour
// import roguelike.model.gamedata.Consumables
// import roguelike.model.gamedata.Melee
// import roguelike.model.gamedata.Ranged
// import roguelike.model.items.Item
// import roguelike.model.GameTile
// import scala.annotation.tailrec

final case class Dungeon(
    playerStart: Point,
    stairsPosition: Point,
    positionedTiles: List[PositionedTile],
    hostiles: List[Hostile],
    collectables: List[Collectable],
    currentFloor: Int
)
