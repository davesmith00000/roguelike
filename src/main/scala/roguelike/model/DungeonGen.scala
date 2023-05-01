package roguelike.model

import indigo.*
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.entity.*
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

import scala.annotation.tailrec

object DungeonGen:

  val RoomMaxSize: Int = 12
  val RoomMinSize: Int = 9
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
        .foldLeft((0.0d, List.empty[(String, Double)])) { case ((total, acc), next) =>
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

  def createRoom(dice: Dice, rect: Rectangle): List[PositionedTile] =
    (rect.top + 1 until rect.bottom).flatMap { y =>
      (rect.left + 1 until rect.right).map { x =>
        val tile =
          if x == rect.left + 1 || x == rect.right - 1 then GameTile.Wall(WallCode.Wall)
          else if y == rect.top + 1 || y == rect.bottom - 1 then GameTile.Wall(WallCode.Wall)
          else GameTile.Ground(dice.rollFromZero(7))

        PositionedTile(Point(x, y), tile)
      }
    }.toList

  def createHorizontalTunnel(
      dice: Dice,
      x1: Int,
      x2: Int,
      y: Int
  ): List[PositionedTile] =
    val start = Math.min(x1, x2)
    val end   = Math.max(x1, x2)

    def column(x: Int) =
      List(
        PositionedTile(Point(x, y - 1), GameTile.Wall(WallCode.Wall)),
        PositionedTile(Point(x, y), GameTile.Ground(dice.rollFromZero(7))),
        PositionedTile(Point(x, y + 1), GameTile.Wall(WallCode.Wall))
      )

    (start to end).toList.flatMap { x =>
      if x == start then
        List(
          PositionedTile(Point(x - 1, y - 1), GameTile.Wall(WallCode.Wall)),
          PositionedTile(Point(x - 1, y + 1), GameTile.Wall(WallCode.Wall))
        ) ++ column(x)
      else if x == end then
        List(
          PositionedTile(Point(x + 1, y - 1), GameTile.Wall(WallCode.Wall)),
          PositionedTile(Point(x + 1, y + 1), GameTile.Wall(WallCode.Wall))
        ) ++ column(x)
      else column(x)
    }

  def createVerticalTunnel(dice: Dice, y1: Int, y2: Int, x: Int): List[PositionedTile] =
    val start = Math.min(y1, y2)
    val end   = Math.max(y1, y2)

    def row(y: Int) =
      List(
        PositionedTile(Point(x - 1, y), GameTile.Wall(WallCode.Wall)),
        PositionedTile(Point(x, y), GameTile.Ground(dice.rollFromZero(7))),
        PositionedTile(Point(x + 1, y), GameTile.Wall(WallCode.Wall))
      )

    (start to end).toList.flatMap { y =>
      if y == start then
        List(
          PositionedTile(Point(x - 1, y - 1), GameTile.Wall(WallCode.Wall)),
          PositionedTile(Point(x + 1, y - 1), GameTile.Wall(WallCode.Wall))
        ) ++ row(y)
      else if y == end then
        List(
          PositionedTile(Point(x - 1, y + 1), GameTile.Wall(WallCode.Wall)),
          PositionedTile(Point(x + 1, y + 1), GameTile.Wall(WallCode.Wall))
        ) ++ row(y)
      else row(y)
    }

  def finaliseTiles(tiles: List[PositionedTile]): List[PositionedTile] =
    @tailrec
    def rec(
        remaining: List[PositionedTile],
        accepted: List[PositionedTile]
    ): List[PositionedTile] =
      remaining match
        case Nil =>
          accepted

        case t :: ts if t.tile.isDownStairs =>
          rec(ts, t :: accepted.filterNot(p => p.position == t.position))

        case t :: ts if t.tile.isGround =>
          accepted.find(_.position == t.position) match
            case None =>
              rec(ts, t :: accepted)

            case Some(tile) if tile.tile.isDownStairs =>
              rec(ts, accepted)

            case Some(tile) =>
              rec(ts, t :: accepted.filterNot(p => p.position == t.position))

        case PositionedTile(pt, GameTile.Wall(code)) :: ts =>
          accepted.find(_.position == pt) match
            case None =>
              rec(ts, PositionedTile(pt, GameTile.Wall(code)) :: accepted)

            case Some(_) =>
              rec(ts, accepted)

        case _ :: ts =>
          rec(ts, accepted)

    rec(tiles, Nil)

  def convertWallsToDropOffs(
      tiles: List[PositionedTile]
  ): List[PositionedTile] =
    @tailrec
    def rec(
        remaining: List[PositionedTile],
        acc: List[PositionedTile]
    ): List[PositionedTile] =
      remaining match
        case Nil =>
          acc

        case head :: next =>
          val others = next ++ acc
          val bm     = others.find(_.position == head.position + Point(0, 1))

          val newWall =
            head.tile match
              case Wall(WallCode.Wall) =>
                val code =
                  if bm.isEmpty then WallCode.DropOff else WallCode.Wall

                PositionedTile(head.position, Wall(code))

              case _ =>
                head

          rec(
            next,
            newWall :: acc
          )

    rec(tiles, Nil)

  def placeCaps(
      tiles: List[PositionedTile]
  ): List[PositionedTile] =
    @tailrec
    def rec(
        remaining: List[PositionedTile],
        acc: List[PositionedTile]
    ): List[PositionedTile] =
      remaining match
        case Nil =>
          acc

        case head :: next =>
          val others = next ++ acc
          val surrounding =
            List(
              others.find(_.position == head.position + Point(-1, -1)).toList,
              others.find(_.position == head.position + Point(0, -1)).toList,
              others.find(_.position == head.position + Point(1, -1)).toList,
              others.find(_.position == head.position + Point(-1, 0)).toList,
              others.find(_.position == head.position + Point(1, 0)).toList
            ).flatten

          val alone = surrounding.map(_.tile).forall {
            case GameTile.Wall(WallCode.DropOff) => true
            case GameTile.Wall(_)                => false
            case _                               => true
          }

          val newWall =
            head.tile match
              case Wall(WallCode.Wall) =>
                val code =
                  if alone then WallCode.Cap else WallCode.Wall

                PositionedTile(head.position, Wall(code))

              case _ =>
                head

          rec(
            next,
            newWall :: acc
          )

    rec(tiles, Nil)

  def placeWallTops(
      tiles: List[PositionedTile]
  ): List[PositionedTile] =
    @tailrec
    def rec(
        remaining: List[PositionedTile],
        acc: List[PositionedTile]
    ): List[PositionedTile] =
      remaining match
        case Nil =>
          acc

        case head :: next =>
          val others = next ++ acc
          val bm     = others.find(_.position == head.position + Point(0, 1)).map(_.tile)

          val newWall =
            head.tile match
              case Wall(WallCode.Wall) =>
                val code =
                  bm match
                    case Some(GameTile.Wall(WallCode.Wall)) =>
                      WallCode.VerticalWallTop

                    case _ =>
                      WallCode.Wall

                PositionedTile(head.position, Wall(code))

              case _ =>
                head

          rec(
            next,
            newWall :: acc
          )

    rec(tiles, Nil)

  def placeWallToCeilingTops(
      tiles: List[PositionedTile]
  ): List[PositionedTile] =
    @tailrec
    def rec(
        remaining: List[PositionedTile],
        acc: List[PositionedTile]
    ): List[PositionedTile] =
      remaining match
        case Nil =>
          acc

        case head :: next =>
          val others = next ++ acc
          val bm     = others.find(_.position == head.position + Point(0, 1)).map(_.tile)

          val newWall =
            head.tile match
              case Wall(WallCode.VerticalWallTop) =>
                val code =
                  bm match
                    case Some(GameTile.Wall(WallCode.Wall)) =>
                      WallCode.VerticalWallToCeilingTop

                    case _ =>
                      WallCode.VerticalWallTop

                PositionedTile(head.position, Wall(code))

              case _ =>
                head

          rec(
            next,
            newWall :: acc
          )

    rec(tiles, Nil)

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
      val processTiles =
        finaliseTiles andThen
          convertWallsToDropOffs andThen
          placeWallTops andThen
          placeWallToCeilingTops andThen
          placeCaps

      if numOfRooms == maxRooms then
        lastRoomCenter match
          case None =>
            val tiles =
              processTiles(roomTiles ++ tunnelTiles)

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
              processTiles(
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
          val newRoomTiles = createRoom(dice, newRoom)
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
                  createHorizontalTunnel(dice, prev.x, roomCenter.x, prev.y) ++
                    createVerticalTunnel(dice, prev.y, roomCenter.y, roomCenter.x)
                else
                  createVerticalTunnel(dice, prev.y, roomCenter.y, prev.x) ++
                    createHorizontalTunnel(dice, prev.x, roomCenter.x, roomCenter.y)

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

final case class Dungeon(
    playerStart: Point,
    stairsPosition: Point,
    positionedTiles: List[PositionedTile],
    hostiles: List[Hostile],
    collectables: List[Collectable],
    currentFloor: Int
)

final case class PositionedTile(position: Point, tile: GameTile)

enum WallCode:
  case Wall
  case DropOff
  case Cap
  case VerticalWallTop
  case VerticalWallToCeilingTop

object WallCode:

  def fromCode(code: String): WallCode =
    code match
      case "d"    => DropOff
      case "w"    => Wall
      case "c"    => Cap
      case "vwt"  => VerticalWallTop
      case "vwct" => VerticalWallToCeilingTop
      case _      => Wall

  extension (wc: WallCode)
    def toCode: String =
      wc match
        case Wall                     => "w"
        case DropOff                  => "d"
        case Cap                      => "c"
        case VerticalWallTop          => "vwt"
        case VerticalWallToCeilingTop => "vwct"
