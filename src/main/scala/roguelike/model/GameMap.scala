package roguelike.model

import indigo._
import indigo.syntax._
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
    tileMap: Batch[Option[GameTile]],
    visible: Batch[Point],
    explored: Set[Point],
    hostiles: HostilesManager,
    collectables: Batch[Collectable]
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
    tileMap.zipWithIndex.collect {
      case (Some(tile), index)
          if tile.isWall && explored.contains(pointFromIndex(index)) =>
        pointFromIndex(index)
    }.toJSArray

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
        explored = explored ++ newVisible.toSet,
        hostiles = es
      )
    }

  def getPathTo(
      dice: Dice,
      from: Point,
      to: Point,
      additionalBlocked: Batch[Point]
  ): Batch[Point] =
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
      this.copy(tileMap = tileMap.update(indexFromPoint(coords), Option(tile)))
    else this

  def insert(tiles: Batch[(Point, GameTile)]): GameMap =
    tiles.foldLeft(this) { case (acc, (pt, tile)) => acc.insert(pt, tile) }

  def insert(tiles: (Point, GameTile)*): GameMap =
    insert(tiles.toBatch)

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
      hostiles: Batch[Hostile],
      collectables: Batch[Collectable]
  ): GameMap =
    GameMap(
      size,
      Batch.fill(size.width * size.height)(None),
      Batch.empty,
      Set(),
      HostilesManager(hostiles),
      collectables
    )

  def gen(size: Size, dungeon: Dungeon): GameMap =
    initial(
      size,
      Batch.fromList(dungeon.hostiles),
      Batch.fromList(dungeon.collectables)
    ).insert(
      dungeon.positionedTiles.toBatch
    )

  def calculateFOV(
      radius: Int,
      center: Point,
      gameMap: GameMap
  ): Batch[Point] =
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
        remaining: Batch[Point],
        acc: Batch[Point]
    ): Batch[Point] =
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

    visibleTiles(tiles.map(_._2), Batch.empty)

  def getPathTo(
      dice: Dice,
      from: Point,
      to: Point,
      additionalBlocked: Batch[Point],
      gameMap: GameMap
  ): Batch[Point] =
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
      walkable: Batch[Point],
      area: Rectangle
  ): Batch[Point] =
    PathFinder
      .fromWalkable(area.size, walkable.map(_ - area.position))
      .locatePath(dice, from - area.position, to - area.position, _ => 1)
      .map(_ + area.position)

  def searchByBoundsWithPosition(
      gameMap: GameMap,
      bounds: Rectangle
  ): Batch[(Point, GameTile)] =
    gameMap.tileMap.zipWithIndex.collect {
      case (Some(tile), index)
          if bounds.contains(Indices.indexToPoint(index, gameMap.size.width)) =>
        (Indices.indexToPoint(index, gameMap.size.width), tile)
    }
