package dungeongen.v2

import dungeongen.classic.DungeonCreation
import indigo.*
import indigo.syntax.*
import roguelike.model.GameTile
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.Dungeon
import roguelike.model.dungeon.DungeonMetadata
import roguelike.model.dungeon.PositionedTile
import roguelike.model.dungeon.WallCode
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

    val roomSpawnPoint = mapSize.toPoint / 2

    val firstRoom = RoomBuilder.makeRoom(dice, roomSpawnPoint, roomMinSize, roomMaxSize)

    val rooms =
      RoomBuilder.moveRooms(
        dice,
        List.fill(maxRooms)(RoomBuilder.makeRoom(dice, roomSpawnPoint, roomMinSize, roomMaxSize)),
        List(firstRoom),
        Rectangle(mapSize)
      )

    val roomTiles =
      rooms.flatMap(_.toPositionedTiles(dice))

    val corridorConnections = findCorridorConnections(rooms.map(_.bounds), dice)

    val newTunnelTiles: List[List[PositionedTile]] =
      corridorConnections.toList
        .map { ls =>
          buildTunnel(dice, ls.start.toPoint, ls.end.toPoint)
        }

    Dungeon(
      playerStart = firstRoom.bounds.center,
      stairsPosition = Point(1),
      positionedTiles = mergeTileSets(roomTiles :: newTunnelTiles),
      hostiles = Nil,
      collectables = Nil,
      currentFloor = 0,
      meta = DungeonMetadata(rooms.map(_.bounds))
    )

  def mergeTileSets(tileSets: List[List[PositionedTile]]): List[PositionedTile] =
    tileSets.foldLeft(List.empty[PositionedTile]) { case (acc, next) =>
      val filtered =
        next.filter {
          case PositionedTile(position, GameTile.DownStairs) =>
            true

          case PositionedTile(position, GameTile.Ground(_)) =>
            true

          case PositionedTile(position, GameTile.Wall(_)) if !acc.exists(_.position == position) =>
            true

          case PositionedTile(position, GameTile.Wall(_)) =>
            false
        }

      acc.filterNot(p => filtered.exists(_.position == p.position)) ++ filtered
    }

  def findCorridorConnections(rooms: List[Rectangle], dice: Dice): Batch[LineSegment] =
    val roomsAsBounds = rooms.toBatch.map(_.contract(1).toBoundingBox)

    val roomPoints    = rooms.toBatch.map(r => r.center)
    val vertices      = roomPoints.map(_.toVertex)
    val superTriangle = Triangle.encompassing(vertices)

    val mesh = Mesh.fromVertices(vertices, superTriangle)

    val goodLines = mesh.toLineSegments.filter {
      case ls @ LineSegment(start, end)
          if roomsAsBounds
            .exists(bb => !bb.contains(start) && !bb.contains(end) && bb.lineIntersects(ls)) =>
        false

      case _ =>
        true
    }

    @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
    @tailrec
    def rec(
        remainingRooms: Batch[BoundingBox],
        remainingLines: Batch[LineSegment],
        acc: Batch[LineSegment]
    ): Batch[LineSegment] =
      remainingRooms match
        case Batch() =>
          acc

        case _ if remainingLines.isEmpty =>
          acc

        case r ==: rs =>
          val (connected, notConnected) =
            remainingLines.partition(ls => r.contains(ls.start) || r.contains(ls.end))

          val res1 =
            connected
              .filter(ls => rs.exists(_.contains(ls.start)) || rs.exists(_.contains(ls.end)))
              .sortBy(_.length)
              .splitAt(dice.roll(2))
              ._1

          val res2 = res1.headOption match
            case None =>
              Batch.empty

            case Some(shortest) =>
              val l = shortest.length
              res1.filter(_.length <= l * 1.25)

          rec(rs, notConnected, res2 ++ acc)

        case _ =>
          throw new Exception("Pattern matching error on Batch of room bounds.")

    val connections = rec(roomsAsBounds, goodLines, Batch.empty)

    connections

  def buildTunnel(dice: Dice, from: Point, to: Point): List[PositionedTile] =
    if dice.roll(2) == 1 then
      mergeTileSets(
        List(
          DungeonCreation.createHorizontalTunnel(dice, from.x, to.x, from.y),
          DungeonCreation.createVerticalTunnel(dice, from.y, to.y, to.x)
        )
      )
    else
      mergeTileSets(
        List(
          DungeonCreation.createVerticalTunnel(dice, from.y, to.y, from.x),
          DungeonCreation.createHorizontalTunnel(dice, from.x, to.x, to.y)
        )
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
