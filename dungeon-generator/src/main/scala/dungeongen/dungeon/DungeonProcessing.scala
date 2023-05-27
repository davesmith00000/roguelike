package dungeongen.dungeon

import indigo.*
import roguelike.model.GameTile
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.PositionedTile
import roguelike.model.dungeon.WallCode

import scala.annotation.tailrec

object DungeonProcessing:

  val processTiles =
    finaliseTiles andThen
      convertWallsToDropOffs andThen
      placeWallTops andThen
      placeWallToCeilingTops andThen
      placeCaps

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
