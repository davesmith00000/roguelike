package roguelike.model

import indigo._
import io.circe._
import io.circe.syntax._
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.model.entity._
import roguelike.util.Indices

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class GameMap(
    size: Size,
    tileMap: js.Array[Option[GameTile]],
    visible: List[Point],
    explored: Set[Point],
    hostiles: HostilesManager,
    collectables: List[Collectable]
):
  val bounds: Rectangle =
    Rectangle(size)

  def noHostilesVisible: Boolean =
    visibleHostiles.isEmpty

  def entitiesList: js.Array[Entity] =
    (collectables.toJSArray ++ hostiles.toJSArray.sortBy(_.isAlive)).filter(e =>
      visible.contains(e.position)
    )

  def visibleHostiles: js.Array[Hostile] =
    hostiles.toJSArray.filter(e => e.isAlive && visible.contains(e.position))

  def visibleSortedHostiles: js.Array[Hostile] =
    hostiles.toJSArray
      .filter(e => visible.contains(e.position))
      .sortBy(_.isAlive)

  def damageHostile(id: Int, damage: Int): Outcome[GameMap] =
    hostiles.damageHostile(id, damage).map { hm =>
      this.copy(hostiles = hm)
    }

  def confuseHostile(id: Int, numberOfTurns: Int): Outcome[GameMap] =
    hostiles.confuseHostile(id, numberOfTurns).map { hm =>
      this.copy(hostiles = hm)
    }

  def exploredWalls: js.Array[Point] =
    tileMap.zipWithIndex
      .collect {
        case (Some(tile), index)
            if tile.isWall && explored.contains(pointFromIndex(index)) =>
          pointFromIndex(index)
      }

  def update(
      dice: Dice,
      playerPosition: Point,
      pause: Boolean
  ): Outcome[GameMap] =
    val newVisible =
      GameMap.calculateFOV(GameMap.FOVRadius, playerPosition, this)

    val updatedEntities =
      hostiles.updateAllHostiles(
        dice,
        playerPosition,
        pause,
        this,
        newVisible
      )

    updatedEntities.map { es =>
      this.copy(
        visible = newVisible,
        explored = explored ++ newVisible,
        hostiles = es
      )
    }

  def getPathTo(
      dice: Dice,
      from: Point,
      to: Point,
      additionalBlocked: List[Point]
  ): List[Point] =
    val area = Rectangle.fromPoints(from, to).expand(2)
    val filter: (GameTile, Point) => Boolean = (tile, _) =>
      tile match
        case GameTile.Ground     => true
        case GameTile.DownStairs => true
        case _                   => false

    val walkable =
      GameMap
        .searchByBoundsWithPosition(this, area)
        .filterNot(_._2.isBlocked)
        .map(_._1)
        .filterNot(additionalBlocked.contains)

    GameMap.getWalkablePathTo(dice, from, to, walkable, area)

  def insert(coords: Point, tile: GameTile): GameMap =
    if bounds.contains(coords) then
      tileMap(indexFromPoint(coords)) = Option(tile)
      this
    else this

  def insert(tiles: List[(Point, GameTile)]): GameMap =
    tiles.foreach { case (pt, tile) =>
      tileMap(indexFromPoint(pt)) = Option(tile)
    }
    this

  def insert(tiles: (Point, GameTile)*): GameMap =
    insert(tiles.toList)

  def lookUp(at: Point): Option[GameTile] =
    if bounds.contains(at) then tileMap(indexFromPoint(at))
    else None

  def dropCollectable(collectable: Collectable): GameMap =
    this.copy(
      collectables = collectable :: collectables
    )

  def toLocalExplored(center: Point, size: Size): js.Array[(GameTile, Point)] =
    val topLeft: Point = center - (size.toPoint / 2)
    val bounds: Rectangle =
      Rectangle.fromPoints(topLeft, topLeft + size.toPoint)

    GameMap
      .searchByBoundsWithPosition(this, bounds)
      .toJSArray
      .map(p => (p._2, p._1))
      .filter((_, pos) => explored.contains(pos))

  private def indexFromPoint(pt: Point): Int =
    Indices.indexFromPoint(pt, size.width)

  private def pointFromIndex(index: Int): Point =
    Indices.pointFromIndex(index, size.width)

object GameMap:

  val FOVRadius: Int = 6

  import SharedCodecs.given

  given Encoder[GameMap] = new Encoder[GameMap] {
    final def apply(data: GameMap): Json =
      ???
    // Json.obj(
    //   ("size", data.size.asJson),
    //   ("tiles", data.tileMap.toListWithPosition.asJson),
    //   ("visible", data.visible.asJson),
    //   ("explored", data.explored.asJson),
    //   ("hostiles", data.hostiles.asJson),
    //   ("collectables", data.collectables.asJson)
    // )
  }

  given Decoder[GameMap] = new Decoder[GameMap] {
    final def apply(c: HCursor): Decoder.Result[GameMap] =
      ???
    // for {
    //   size         <- c.downField("size").as[Size]
    //   tiles        <- c.downField("tiles").as[List[(Point, GameTile)]]
    //   visible      <- c.downField("visible").as[List[Point]]
    //   explored     <- c.downField("explored").as[Set[Point]]
    //   hostiles     <- c.downField("hostiles").as[List[Hostile]]
    //   collectables <- c.downField("collectables").as[List[Collectable]]
    // } yield GameMap(
    //   size,
    //   QuadTree.empty(size.width, size.height),
    //   visible,
    //   explored,
    //   hostiles,
    //   collectables
    // ).insert(tiles)
  }

  def initial(
      size: Size,
      hostiles: List[Hostile],
      collectables: List[Collectable]
  ): GameMap =
    GameMap(
      size,
      List.fill(size.width * size.height)(None).toJSArray,
      Nil,
      Set(),
      HostilesManager(hostiles),
      collectables
    )

  def gen(size: Size, dungeon: Dungeon): GameMap =
    initial(size, dungeon.hostiles, dungeon.collectables).insert(
      dungeon.positionedTiles
    )

  def calculateFOV(
      radius: Int,
      center: Point,
      gameMap: GameMap
  ): List[Point] =
    val bounds: Rectangle =
      Rectangle(
        (center - radius).max(0),
        (Size(center.x, center.y) + radius).max(1)
      )

    val tiles =
      searchByBoundsWithPosition(gameMap, bounds)
        .map(p => (p._2, p._1))
        .filter(t => center.distanceTo(t._2) <= radius)

    @tailrec
    def visibleTiles(
        remaining: js.Array[Point],
        acc: List[Point]
    ): List[Point] =
      remaining match
        case l if l.isEmpty =>
          acc

        case l =>
          val lineOfSight = FOV.bresenhamLine(l.head, center).dropRight(1)

          if lineOfSight.forall(pt =>
              tiles.exists(t => t._2 == pt && !t._1.blockSight)
            )
          then
            visibleTiles(
              l.tail,
              l.head :: acc
            )
          else visibleTiles(l.tail, acc)

    visibleTiles(tiles.map(_._2), Nil)

  def getPathTo(
      dice: Dice,
      from: Point,
      to: Point,
      additionalBlocked: List[Point],
      gameMap: GameMap
  ): List[Point] =
    val area = Rectangle.fromPoints(from, to).expand(2)
    val filter: (GameTile, Point) => Boolean = (tile, _) =>
      tile match
        case GameTile.Ground     => true
        case GameTile.DownStairs => true
        case _                   => false

    val walkable =
      searchByBoundsWithPosition(gameMap, area)
        .filterNot(_._2.isBlocked)
        .map(_._1)
        .filterNot(additionalBlocked.contains)

    GameMap.getWalkablePathTo(dice, from, to, walkable, area)

  def getWalkablePathTo(
      dice: Dice,
      from: Point,
      to: Point,
      walkable: js.Array[Point],
      area: Rectangle
  ): List[Point] =
    PathFinder
      .fromWalkable(area.size, walkable.map(_ - area.position).toList)
      .locatePath(dice, from - area.position, to - area.position, _ => 1)
      .map(_ + area.position)

  def searchByBoundsWithPosition(
      gameMap: GameMap,
      bounds: Rectangle
  ): js.Array[(Point, GameTile)] =
    gameMap.tileMap.zipWithIndex.collect {
      case (Some(tile), index)
          if bounds.contains(Indices.indexToPoint(index, gameMap.size.width)) =>
        (Indices.indexToPoint(index, gameMap.size.width), tile)
    }