package dungeongen.classic

import indigo.*
import roguelike.model.GameTile
import roguelike.model.dungeon.PositionedTile
import roguelike.model.dungeon.WallCode

object DungeonCreation:

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
