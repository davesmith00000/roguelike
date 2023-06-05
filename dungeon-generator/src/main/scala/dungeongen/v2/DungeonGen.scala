package dungeongen.v2

import indigo.*
import roguelike.model.GameTile
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.Dungeon
import roguelike.model.dungeon.PositionedTile
import roguelike.model.dungeon.WallCode
import roguelike.model.entity.*
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

object DungeonGen:

  def makeMap(
      dice: Dice,
      maxRooms: Int,
      roomMinSize: Int,
      roomMaxSize: Int,
      mapSize: Size,
      maxMonstersPerRoom: Int,
      maxCollectablesPerRoom: Int,
      currentFloor: Int
  ): Dungeon =

    val roomSpawnPoint = mapSize.toPoint / 2

    val firstRoom = RoomBuilder.makeRoom(dice, roomSpawnPoint, roomMinSize, roomMaxSize)

    val rooms =
      RoomBuilder.moveRooms(
        dice,
        List.fill(maxRooms)(RoomBuilder.makeRoom(dice, roomSpawnPoint, roomMinSize, roomMaxSize)),
        List(firstRoom),
        Rectangle(mapSize)
      )

    Dungeon(
      playerStart = firstRoom.bounds.center,
      stairsPosition = Point(1),
      positionedTiles = rooms.flatMap(_.toPositionedTiles(dice)),
      hostiles = Nil,
      collectables = Nil,
      currentFloor = 0
    )

final case class Room(bounds: Rectangle):
  def toPositionedTiles(dice: Dice): List[PositionedTile] =
    val rect = bounds.contract(1)
    (rect.top until rect.bottom).flatMap { y =>
      (rect.left until rect.right).map { x =>
        val tile =
          if x == rect.left || x == rect.right - 1 then GameTile.Wall(WallCode.Wall)
          else if y == rect.top || y == rect.bottom - 1 then GameTile.Wall(WallCode.Wall)
          else GameTile.Ground(dice.rollFromZero(7))

        PositionedTile(Point(x, y), tile)
      }
    }.toList
