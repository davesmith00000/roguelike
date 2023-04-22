package roguelike.model

import indigo.Dice
import indigo.Point
import indigo.Rectangle
import indigo.Size
import indigo.shared.collections.Batch

import scalajs.js
import scalajs.js.JSConverters.*

class GameMapTests extends munit.FunSuite {

  val walkable =
    Batch(
      Point(4, 6),
      Point(3, 6),
      Point(4, 5),
      Point(3, 5),
      Point(4, 4),
      Point(3, 4),
      Point(4, 3),
      Point(3, 3),
      Point(3, 2),
      Point(2, 6),
      Point(1, 6),
      Point(0, 6),
      Point(2, 5),
      Point(1, 5),
      Point(0, 5),
      Point(2, 4),
      Point(1, 4),
      Point(2, 3),
      Point(1, 3),
      Point(0, 4),
      Point(0, 3),
      Point(2, 2),
      Point(3, 1),
      Point(3, 0),
      Point(2, 1),
      Point(2, 0)
    )

  test("getPathTo") {
    val gameMap =
      GameMap
        .initial(Size(5, 7))
        .insert(walkable.map(pt => PositionedTile(pt, GameTile.Ground(0))))

    val possiblePaths: List[List[Point]] =
      List(
        List(
          Point(3, 2),
          Point(2, 2),
          Point(2, 3),
          Point(2, 4),
          Point(2, 5)
        ),
        List(
          Point(3, 2),
          Point(3, 3),
          Point(3, 4),
          Point(2, 4),
          Point(2, 5)
        ),
        List(
          Point(3, 2),
          Point(3, 3),
          Point(3, 4),
          Point(3, 5),
          Point(2, 5)
        )
      )

    val actual =
      GameMap.getPathTo(
        Point(3, 2),
        Point(2, 5),
        Batch.empty,
        gameMap
      )

    assert(possiblePaths.contains(actual.toList))
  }

  test("searchByBoundsWithPosition - empty") {
    val mapSize = Size(10)
    val bounds  = Rectangle(1, 1, 3, 3)
    val gameMap = GameMap.initial(mapSize)

    val actual =
      GameMap.searchByBoundsWithPosition(gameMap, bounds)

    val expected =
      js.Array[(Point, GameTile)]()

    assertEquals(actual.toList, expected.toList)
  }

  test("searchByBoundsWithPosition - 1 item in bounds") {
    val mapSize = Size(10)
    val bounds  = Rectangle(1, 1, 3, 3)

    val gameMap =
      GameMap
        .initial(mapSize)
        .insert(Point(1, 1), GameTile.Wall(None))

    val actual =
      GameMap.searchByBoundsWithPosition(gameMap, bounds)

    val expected =
      js.Array[(Point, GameTile)]((Point(1, 1), GameTile.Wall(None)))

    assertEquals(actual.toList, expected.toList)
  }

  test("searchByBoundsWithPosition - some in bounds, some out") {
    val mapSize = Size(5)
    val bounds  = Rectangle(1, 2, 3, 2)

    /*
|X|_|_|_|_|
|_|_|_|X|_|
|X|_|X|_|_|
|_|X|_|_|_|
|_|_|X|_|X|
     */
    val gameMap =
      GameMap
        .initial(mapSize)
        .insert(Point(0, 0), GameTile.Wall(None)) // Out
        .insert(Point(0, 2), GameTile.Wall(None)) // Out
        .insert(Point(1, 3), GameTile.Wall(None)) // In
        .insert(Point(2, 2), GameTile.Ground(0))  // In
        .insert(Point(2, 4), GameTile.Wall(None)) // Out
        .insert(Point(3, 1), GameTile.Wall(None)) // Out
        .insert(Point(4, 4), GameTile.Wall(None)) // Out

    val actual =
      GameMap.searchByBoundsWithPosition(gameMap, bounds)

    val expected =
      js.Array[(Point, GameTile)](
        (Point(1, 3), GameTile.Wall(None)),
        (Point(2, 2), GameTile.Ground(0))
      )

    assert(actual.length == expected.length)
    assert(actual.forall(a => expected.contains(a)))
  }

}
