package roguelike.model

import indigo._
import roguelike.model.entity._
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

import scala.annotation.tailrec
import scala.scalajs.js

object DungeonGen:

  val RoomMaxSize: Int = 10
  val RoomMinSize: Int = 6
  val MaxRooms: Int    = 30

  final case class Limit(floor: Int, amount: Int)
  final case class Chance(entity: String, weight: Int)

  val maxCollectablesByFloor: List[Limit] = List(
    Limit(0, 1),
    Limit(3, 2)
  )

  val maxMonstersByFloor: List[Limit] = List(
    Limit(0, 2),
    Limit(3, 3),
    Limit(5, 5)
  )

  def maxCollectablesPerRoom(floor: Int): Int =
    maxCollectablesByFloor.foldLeft(2) { case (num, limit) =>
      if limit.floor <= floor then limit.amount else num
    }

  def maxMonstersPerRoom(floor: Int): Int =
    maxMonstersByFloor.foldLeft(2) { case (num, limit) =>
      if limit.floor <= floor then limit.amount else num
    }

  def collectableChances: Map[Int, List[Chance]] = Map(
    0 -> List(Chance(Consumables.HealthPotion.name, 35)),
    2 -> List(Chance(Ranged.ConfusionScroll.name, 10)),
    4 -> List(
      Chance(Ranged.LightningScroll.name, 25),
      Chance(Melee.Sword.name, 5)
    ),
    6 -> List(
      Chance(Ranged.FireballScroll.name, 25),
      Chance(Armour.ChainMail.name, 15)
    )
  )

  def enemyChances: Map[Int, List[Chance]] = Map(
    0 -> List(Chance(Orc.name, 80)),
    3 -> List(Chance(Troll.name, 15)),
    5 -> List(Chance(Troll.name, 30)),
    7 -> List(Chance(Troll.name, 60))
  )

  def randomChoices(
      dice: Dice,
      count: Int,
      floor: Int,
      chances: Map[Int, List[Chance]]
  ): List[String] =
    @tailrec
    def select(
        remaining: List[(Int, List[Chance])],
        acc: Map[String, Chance]
    ): List[Chance] =
      remaining match
        case Nil =>
          acc.toList.map(_._2)

        case (flr, _) :: xs if flr > floor =>
          select(Nil, acc)

        case (_, cs) :: xs =>
          select(xs, acc ++ cs.map(c => (c.entity, c)).toMap)

    val possibilities: List[Chance] = select(chances.toList, Map())

    val normalised: List[(String, Double)] =
      val total = possibilities.map(_.weight).sum

      val l = possibilities
        .map(p => (p.entity, p.weight.toDouble / total.toDouble))
        .sortBy(_._2)
        .foldLeft((0.0d, List.empty[(String, Double)])) {
          case ((total, acc), next) =>
            (total + next._2, acc :+ (next._1, total + next._2))
        }
        ._2

      l.dropRight(1) ++ l.reverse.headOption.map(e => (e._1, 1.0)).toList

    @tailrec
    def pick(remaining: List[(String, Double)], roll: Double): String =
      remaining match
        case Nil =>
          "" // shouldn't happen...

        case (name, chance) :: xs if roll <= chance =>
          name

        case _ :: xs =>
          pick(xs, roll)

    (0 until count).toList.map { _ =>
      pick(normalised, dice.rollDouble)
    }

  def placeEntities(
      floor: Int,
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxMonstersPerRoom: Int
  ): List[Hostile] =
    randomChoices(
      dice,
      maxMonstersPerRoom,
      floor,
      enemyChances
    ).zipWithIndex.flatMap {
      case (Orc.name, i) =>
        val x = dice.rollFromZero(room.width - 4) + room.left + 2
        val y = dice.rollFromZero(room.height - 4) + room.top + 2

        List(Orc.spawn(entityCount + i, Point(x, y)))

      case (Troll.name, i) =>
        val x = dice.rollFromZero(room.width - 4) + room.left + 2
        val y = dice.rollFromZero(room.height - 4) + room.top + 2

        List(Troll.spawn(entityCount + i, Point(x, y)))

      case _ =>
        Nil
    }.distinct

  def placeCollectables(
      floor: Int,
      entityCount: Int,
      dice: Dice,
      room: Rectangle,
      maxCollectablesPerRoom: Int,
      hostiles: List[Hostile],
      roomCenter: Point
  ): List[Collectable] =
    def spawn(item: Item): List[Collectable] =
      val x   = dice.rollFromZero(room.width - 4) + room.left + 2
      val y   = dice.rollFromZero(room.height - 4) + room.top + 2
      val pos = Point(x, y)

      if hostiles.contains(pos) || pos == roomCenter then Nil
      else List(Collectable(pos, item))

    randomChoices(
      dice,
      maxCollectablesPerRoom,
      floor,
      collectableChances
    ).flatMap {
      case Consumables.HealthPotion.name =>
        spawn(Consumables.HealthPotion)

      case Ranged.FireballScroll.name =>
        spawn(Ranged.FireballScroll)

      case Ranged.ConfusionScroll.name =>
        spawn(Ranged.ConfusionScroll)

      case Ranged.LightningScroll.name =>
        spawn(Ranged.LightningScroll)

      case Melee.Dagger.name =>
        spawn(Melee.Dagger)

      case Melee.Sword.name =>
        spawn(Melee.Sword)

      case Armour.LeatherArmor.name =>
        spawn(Armour.LeatherArmor)

      case Armour.ChainMail.name =>
        spawn(Armour.ChainMail)

      case _ =>
        Nil
    }.distinct

  def createRoom(rect: Rectangle): List[(Point, GameTile)] =
    (rect.top + 1 until rect.bottom).flatMap { y =>
      (rect.left + 1 until rect.right).map { x =>
        val tile =
          if x == rect.left + 1 || x == rect.right - 1 then GameTile.Wall
          else if y == rect.top + 1 || y == rect.bottom - 1 then GameTile.Wall
          else GameTile.Ground

        (Point(x, y), tile)
      }
    }.toList
  def createRoom(
      x: Int,
      y: Int,
      width: Int,
      height: Int
  ): List[(Point, GameTile)] =
    createRoom(Rectangle(x, y, width, height))

  def createHorizontalTunnel(
      x1: Int,
      x2: Int,
      y: Int
  ): List[(Point, GameTile)] =
    val start = Math.min(x1, x2)
    val end   = Math.max(x1, x2)
    (start to end).toList.flatMap { x =>
      if x == start then
        List(
          (Point(x - 1, y - 1), GameTile.Wall),
          (Point(x - 1, y + 1), GameTile.Wall)
        ) ++
          List(
            (Point(x, y - 1), GameTile.Wall),
            (Point(x, y), GameTile.Ground),
            (Point(x, y + 1), GameTile.Wall)
          )
      else if x == end then
        List(
          (Point(x + 1, y - 1), GameTile.Wall),
          (Point(x + 1, y + 1), GameTile.Wall)
        ) ++
          List(
            (Point(x, y - 1), GameTile.Wall),
            (Point(x, y), GameTile.Ground),
            (Point(x, y + 1), GameTile.Wall)
          )
      else
        List(
          (Point(x, y - 1), GameTile.Wall),
          (Point(x, y), GameTile.Ground),
          (Point(x, y + 1), GameTile.Wall)
        )
    }

  def createVerticalTunnel(y1: Int, y2: Int, x: Int): List[(Point, GameTile)] =
    val start = Math.min(y1, y2)
    val end   = Math.max(y1, y2)
    (start to end).toList.flatMap { y =>
      if y == start then
        List(
          (Point(x - 1, y - 1), GameTile.Wall),
          (Point(x + 1, y - 1), GameTile.Wall)
        ) ++
          List(
            (Point(x - 1, y), GameTile.Wall),
            (Point(x, y), GameTile.Ground),
            (Point(x + 1, y), GameTile.Wall)
          )
      else if y == end then
        List(
          (Point(x - 1, y + 1), GameTile.Wall),
          (Point(x + 1, y + 1), GameTile.Wall)
        ) ++
          List(
            (Point(x - 1, y), GameTile.Wall),
            (Point(x, y), GameTile.Ground),
            (Point(x + 1, y), GameTile.Wall)
          )
      else
        List(
          (Point(x - 1, y), GameTile.Wall),
          (Point(x, y), GameTile.Ground),
          (Point(x + 1, y), GameTile.Wall)
        )
    }

  @tailrec
  def finaliseTiles(
      remaining: List[(Point, GameTile)],
      accepted: List[(Point, GameTile)]
  ): List[(Point, GameTile)] =
    remaining match
      case Nil =>
        accepted

      case t :: ts if t._2.isDownStairs =>
        finaliseTiles(ts, t :: accepted.filterNot(p => p._1 == t._1))

      case t :: ts if t._2.isGround =>
        accepted.find(_._1 == t._1) match
          case None =>
            finaliseTiles(ts, t :: accepted)

          case Some(tile) if tile._2.isDownStairs =>
            finaliseTiles(ts, accepted)

          case Some(tile) =>
            finaliseTiles(ts, t :: accepted.filterNot(p => p._1 == t._1))

      case (pt, GameTile.Wall) :: ts =>
        accepted.find(_._1 == pt) match
          case None =>
            finaliseTiles(ts, (pt, GameTile.Wall) :: accepted)

          case Some(_) =>
            finaliseTiles(ts, accepted)

      case _ :: ts =>
        finaliseTiles(ts, accepted)

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
        roomTiles: List[(Point, GameTile)],
        tunnelTiles: List[(Point, GameTile)],
        hostiles: List[Hostile],
        collectables: List[Collectable],
        playerStart: Point,
        stairsPosition: Point
    ): Dungeon =
      if numOfRooms == maxRooms then
        lastRoomCenter match
          case None =>
            new Dungeon {
              val playerStart: Point    = playerStart
              val stairsPosition: Point = stairsPosition
              val positionedTiles: List[(Point, GameTile)] =
                finaliseTiles(roomTiles ++ tunnelTiles, Nil)
              val hostiles: List[Hostile]         = hostiles
              val collectables: List[Collectable] = collectables
              val currentFloor: Int               = currentFloor
            }

          case Some(center) =>
            new Dungeon {
              val playerStart: Point    = playerStart
              val stairsPosition: Point = stairsPosition
              val positionedTiles: List[(Point, GameTile)] =
                finaliseTiles(
                  roomTiles ++ tunnelTiles ++ List(
                    (center, GameTile.DownStairs)
                  ),
                  Nil
                )
              val hostiles: List[Hostile]         = hostiles
              val collectables: List[Collectable] = collectables
              val currentFloor: Int               = currentFloor
            }
      else
        val w = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val h = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
        val x = dice.rollFromZero(mapSize.width - w - 1)
        val y = dice.rollFromZero(mapSize.height - h - 1)

        val newRoom = Rectangle(x, y, w, h)

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
          val newRoomTiles = createRoom(newRoom)
          val roomCenter   = newRoom.center
          val roomHostiles =
            if numOfRooms == 0 then Nil
            else
              placeEntities(
                currentFloor,
                hostiles.length,
                dice,
                newRoom,
                maxMonstersPerRoom
              )
          val roomCollectables =
            if numOfRooms == 0 then Nil
            else
              placeCollectables(
                currentFloor,
                collectables.length,
                dice,
                newRoom,
                maxCollectablesPerRoom,
                roomHostiles,
                roomCenter
              )

          val newTunnelTiles =
            lastRoomCenter match
              case None =>
                Nil

              case Some(prev) =>
                if dice.roll(2) == 1 then
                  createHorizontalTunnel(prev.x, roomCenter.x, prev.y) ++
                    createVerticalTunnel(prev.y, roomCenter.y, roomCenter.x)
                else
                  createVerticalTunnel(prev.y, roomCenter.y, prev.x) ++
                    createHorizontalTunnel(prev.x, roomCenter.x, roomCenter.y)

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

trait Dungeon:
  val playerStart: Point
  val stairsPosition: Point
  val positionedTiles: List[(Point, GameTile)]
  val hostiles: List[Hostile]
  val collectables: List[Collectable]
  val currentFloor: Int
