package roguelike.model

import indigo.Dice
import indigo.Point
import indigo.Rectangle
import indigo.Size
import indigo.shared.collections.Batch

import scalajs.js
import scalajs.js.JSConverters._

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
    ).map(_ + Point(17, 21))

  test("getWalkablePathTo") {
    val actual =
      GameMap.getWalkablePathTo(
        Dice.fromSeed(0),
        Point(20, 23),
        Point(19, 26),
        walkable,
        Rectangle(Point(17, 21), Size(5, 7))
      )

    val possiblePaths: List[List[Point]] =
      List(
        List(
          Point(20, 23),
          Point(19, 23),
          Point(19, 24),
          Point(19, 25),
          Point(19, 26)
        ),
        List(
          Point(20, 23),
          Point(20, 24),
          Point(20, 25),
          Point(19, 25),
          Point(19, 26)
        )
      )

    assert(possiblePaths.contains(actual.toList))
  }

  test("searchByBoundsWithPosition - empty") {
    val mapSize = Size(10)
    val bounds  = Rectangle(1, 1, 3, 3)
    val gameMap = GameMap.initial(mapSize, Batch.empty)

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
      GameMap.initial(mapSize, Batch.empty)
        .insert(Point(1, 1), GameTile.Wall)

    val actual =
      GameMap.searchByBoundsWithPosition(gameMap, bounds)

    val expected =
      js.Array[(Point, GameTile)]((Point(1, 1), GameTile.Wall))

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
      GameMap.initial(mapSize, Batch.empty)
        .insert(Point(0, 0), GameTile.Wall) // Out
        .insert(Point(0, 2), GameTile.Wall) // Out
        .insert(Point(1, 3), GameTile.Wall) // In
        .insert(Point(2, 2), GameTile.Ground) // In
        .insert(Point(2, 4), GameTile.Wall) // Out
        .insert(Point(3, 1), GameTile.Wall) // Out
        .insert(Point(4, 4), GameTile.Wall) // Out

    val actual =
      GameMap.searchByBoundsWithPosition(gameMap, bounds)

    val expected =
      js.Array[(Point, GameTile)]((Point(1, 3), GameTile.Wall), (Point(2, 2), GameTile.Ground))

    assert(actual.length == expected.length)
    assert(actual.forall(a => expected.contains(a)))
  }

}
