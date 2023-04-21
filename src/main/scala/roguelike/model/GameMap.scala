package roguelike.model

import indigo.*
import indigo.syntax.*
import io.circe.*
import io.circe.syntax.*
import io.indigoengine.roguelike.starterkit.utils.FOV
import roguelike.GameEvent
import roguelike.model.entity.*
import roguelike.util.Indices
import roguelike.util.PathFinder

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

final case class GameMap(
    size: Size,
    tileMap: Batch[Option[GameTile]],
    visible: Batch[Point],
    explored: Set[Point]
):
  val bounds: Rectangle =
    Rectangle(size)

  def exploredWalls: js.Array[Point] =
    tileMap.zipWithIndex.collect {
      case (Some(tile), index) if tile.isWall && explored.contains(pointFromIndex(index)) =>
        pointFromIndex(index)
    }.toJSArray

  def update(playerPosition: Point): GameMap =
    val newVisible =
      GameMap.calculateFOV(GameMap.FOVRadius, playerPosition, this)

    this.copy(
      visible = newVisible,
      explored = explored ++ newVisible.toSet
    )

  def getPathTo(
      from: Point,
      to: Point,
      additionalBlocked: Batch[Point]
  ): Batch[Point] =
    GameMap.getPathTo(from, to, additionalBlocked, this)

  def insert(coords: Point, tile: GameTile): GameMap =
    if bounds.contains(coords) then
      this.copy(tileMap = tileMap.update(indexFromPoint(coords), Option(tile)))
    else this

  def insert(tiles: Batch[PositionedTile]): GameMap =
    tiles.foldLeft(this) { case (acc, positionedTile) =>
      acc.insert(positionedTile.position, positionedTile.tile)
    }

  def insert(tiles: PositionedTile*): GameMap =
    insert(tiles.toBatch)

  def lookUp(at: Point): Option[GameTile] =
    if bounds.contains(at) then tileMap(indexFromPoint(at))
    else None

  def toLocalExplored(center: Point, size: Size): Batch[(GameTile, Point)] =
    val topLeft: Point = center - (size.toPoint / 2)
    val bounds: Rectangle =
      Rectangle.fromPoints(topLeft, topLeft + size.toPoint)

    GameMap
      .searchByBoundsWithPosition(this, bounds)
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

  def initial(size: Size): GameMap =
    GameMap(
      size,
      Batch.fill(size.width * size.height)(None),
      Batch.empty,
      Set()
    )

  def gen(size: Size, dungeon: Dungeon): GameMap =
    initial(size).insert(dungeon.positionedTiles.toBatch)

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

          if lineOfSight.forall(pt => tiles.exists(t => t._2 == pt && !t._1.blockSight))
          then
            visibleTiles(
              l.drop(1),
              l.head :: acc
            )
          else visibleTiles(l.drop(1), acc)

    visibleTiles(tiles.map(_._2), Batch.empty)

  def getPathTo(
      from: Point,
      to: Point,
      additionalBlocked: Batch[Point],
      gameMap: GameMap
  ): Batch[Point] =
    // Bit of a premature optimisation, tries to find a very direct path by limiting the search scope drastically...
    val area = Rectangle.fromPoints(from, to).expand(4)

    val walkable =
      searchByBoundsWithPosition(gameMap, area)
        .filterNot(_._2.isBlocked)
        .map(_._1)
        .filterNot(additionalBlocked.contains)

    PathFinder.locatePath(from, to, walkable)

  def searchByBoundsWithPosition(
      gameMap: GameMap,
      bounds: Rectangle
  ): Batch[(Point, GameTile)] =
    gameMap.tileMap.zipWithIndex
      .map { case (maybeGameTile, index) =>
        (maybeGameTile, Indices.indexToPoint(index, gameMap.size.width))
      }
      .collect {
        case (Some(tile), position) if bounds.contains(position) =>
          (position, tile)
      }
