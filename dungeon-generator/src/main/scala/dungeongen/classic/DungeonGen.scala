package dungeongen.classic

import indigo.*
import roguelike.model.GameTile
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.Dungeon
import roguelike.model.dungeon.PositionedTile
import roguelike.model.entity.*
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

import scala.annotation.tailrec

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
    @tailrec
    def rec(
        numOfRooms: Int,
        lastRoomCenter: Option[Point],
        rooms: List[Rectangle],
        roomTiles: List[PositionedTile],
        tunnelTiles: List[PositionedTile],
        hostiles: List[Hostile],
        collectables: List[Collectable],
        playerStart: Point,
        stairsPosition: Point
    ): Dungeon =
      if numOfRooms == maxRooms then
        lastRoomCenter match
          case None =>
            val tiles =
              DungeonProcessing.processTiles(roomTiles ++ tunnelTiles)

            Dungeon(
              playerStart,
              stairsPosition,
              tiles,
              hostiles,
              collectables,
              currentFloor
            )

          case Some(center) =>
            val tiles =
              DungeonProcessing.processTiles(
                roomTiles ++ tunnelTiles ++ List(PositionedTile(center, GameTile.DownStairs))
              )

            Dungeon(
              playerStart,
              center,
              tiles,
              hostiles,
              collectables,
              currentFloor
            )
      else
        val w = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val h = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val x = dice.rollFromZero(mapSize.width - w - 1)
        val y = dice.rollFromZero(mapSize.height - h - 1)

        val newRoom = Rectangle(x, y, w - 2, h - 2).moveBy(1, 1)

        if rooms.exists(_.overlaps(newRoom)) then
          rec(
            numOfRooms + 1,
            lastRoomCenter,
            rooms,
            roomTiles,
            tunnelTiles,
            hostiles,
            collectables,
            playerStart,
            stairsPosition
          )
        else
          val newRoomTiles = DungeonCreation.createRoom(dice, newRoom)
          val roomCenter   = newRoom.center
          val roomHostiles =
            if numOfRooms == 0 then Nil
            else
              DungeonPlacement.placeEntities(
                currentFloor,
                hostiles.length,
                dice,
                newRoom,
                maxMonstersPerRoom
              )
          val roomCollectables =
            if numOfRooms == 0 then Nil
            else
              DungeonPlacement.placeCollectables(
                currentFloor,
                collectables.length,
                dice,
                newRoom.contract(1),
                maxCollectablesPerRoom,
                roomHostiles,
                roomCenter
              )

          val newTunnelTiles: List[PositionedTile] =
            lastRoomCenter match
              case None =>
                Nil

              case Some(prev) =>
                if dice.roll(2) == 1 then
                  DungeonCreation.createHorizontalTunnel(dice, prev.x, roomCenter.x, prev.y) ++
                    DungeonCreation.createVerticalTunnel(dice, prev.y, roomCenter.y, roomCenter.x)
                else
                  DungeonCreation.createVerticalTunnel(dice, prev.y, roomCenter.y, prev.x) ++
                    DungeonCreation.createHorizontalTunnel(dice, prev.x, roomCenter.x, roomCenter.y)

          rec(
            numOfRooms + 1,
            Option(roomCenter),
            newRoom :: rooms,
            roomTiles ++ newRoomTiles,
            tunnelTiles ++ newTunnelTiles,
            hostiles ++ roomHostiles,
            collectables ++ roomCollectables,
            if numOfRooms == 0 then roomCenter else playerStart,
            stairsPosition
          )

    rec(0, None, Nil, Nil, Nil, Nil, Nil, Point.zero, Point.zero)
