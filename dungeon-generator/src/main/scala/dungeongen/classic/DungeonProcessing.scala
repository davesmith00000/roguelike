package dungeongen.classic

import indigo.*
import roguelike.model.GameTile
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.PositionedTile
import roguelike.model.dungeon.WallCode

import scala.annotation.tailrec

object DungeonProcessing:

  val processTiles: List[PositionedTile] => List[PositionedTile] =
    finaliseTiles andThen groupWithSurrounding andThen chooseTile

  def chooseTile: List[(PositionedTile, List[Option[GameTile]])] => List[PositionedTile] =
    _.map {

      /*
      wgg
      w_w
      ...
       */
      case (
            t,
            List(
              Some(Wall(_)),
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Wall(_)),
              None,
              Some(Wall(_)),
              None,
              None,
              None
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOffFadeLeft))

      /*
      ggw
      w_w
      ...
       */
      case (
            t,
            List(
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Wall(_)),
              Some(Wall(_)),
              None,
              Some(Wall(_)),
              None,
              None,
              None
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOffFadeRight))

      /*
      gw.
      w_.
      ...
       */
      case (
            t,
            List(
              Some(Ground(_)),
              Some(Wall(_)),
              None,
              Some(Wall(_)),
              None,
              None,
              None,
              None,
              None
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOff))

      /*
      .wg
      ._w
      ...
       */
      case (
            t,
            List(
              None,
              Some(Wall(_)),
              Some(Ground(_)),
              None,
              None,
              Some(Wall(_)),
              None,
              None,
              None
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOff))

      /*
      ggg
      w_w
      ..w
       */
      case (
            t,
            List(
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Wall(_)),
              None,
              Some(Wall(_)),
              None,
              None,
              Some(Wall(_))
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOffMiddle))

      /*
      ggg
      w_w
      w..
       */
      case (
            t,
            List(
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Wall(_)),
              None,
              Some(Wall(_)),
              Some(Wall(_)),
              None,
              None
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOffMiddle))

      /*
      ggg
      w_w
      ...
       */
      case (
            t,
            List(
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Ground(_)),
              Some(Wall(_)),
              None,
              Some(Wall(_)),
              None,
              None,
              None
            )
          ) =>
        t.copy(tile = Wall(WallCode.DropOffMiddle))

      // Catch all: If there is ground above you, fall back to a drop off so
      // that we don't obscure the path.
      case (
            t,
            List(
              _,
              Some(Ground(_)),
              _,
              _,
              None,
              _,
              _,
              _,
              _
            )
          ) if t.tile.isWall =>
        t.copy(tile = Wall(WallCode.DropOff))

      case (t, List(_, _, _, _, None, _, _, _, _)) =>
        t

      case (t, _) =>
        t
    }

  def groupWithSurrounding: List[PositionedTile] => List[(PositionedTile, List[Option[GameTile]])] =
    tiles =>
      def extractAt(pos: Point): Option[GameTile] =
        tiles.find(_.position == pos).map(_.tile)

      tiles.map { t =>
        t -> List(
          extractAt(t.position + Point(-1, -1)), // tl
          extractAt(t.position + Point(0, -1)),  // tm
          extractAt(t.position + Point(1, -1)),  // tr
          extractAt(t.position + Point(-1, 0)),  // l
          None,                                  // c
          extractAt(t.position + Point(1, 0)),   // r
          extractAt(t.position + Point(-1, 1)),  // bl
          extractAt(t.position + Point(0, 1)),   // bm
          extractAt(t.position + Point(1, 1))    // br
        )
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
