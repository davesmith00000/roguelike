package roguelike.util

import indigo.shared.collections.Batch
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Size

class PathFinderTests extends munit.FunSuite {

  test("Node path contains point") {
    val node = Node.Move(Point(0, 1), Node.Root(Point(0, 0)))

    assert(node.contains(Point(0, 1)))
    assert(node.contains(Point(0, 0)))
    assert(!node.contains(Point(2, 0)))
  }

  test("Finding an unobscured path.should be able to find a route") {
    val start: Point = Point(2, 1)
    val end: Point   = Point(0, 2)
    val walkable: Batch[Point] =
      Batch(
        Point(0, 0),
        Point(1, 0),
        Point(2, 0),
        Point(0, 1),
        Point(1, 1),
        Point(2, 1),
        Point(0, 2),
        Point(1, 2),
        Point(2, 2)
      ).filterNot(_ == Point(1, 0))

    val path: Batch[Point] =
      PathFinder.locatePath(start, end, walkable)

    val possiblePaths: List[List[Point]] = List(
      List(start, Point(2, 2), Point(1, 2), end),
      List(start, Point(1, 1), Point(0, 1), end),
      List(start, Point(1, 1), Point(1, 2), end)
    )

    assertEquals(possiblePaths.contains(path.toList), true)
  }

  test("No path") {
    val start: Point = Point(0, 0)
    val end: Point   = Point(2, 2)

    // Wall in the way down column (1)
    val walkable: Batch[Point] =
      Batch(
        Point(0, 0),
        // Point(1, 0),
        Point(2, 0),
        Point(0, 1),
        // Point(1, 1),
        Point(2, 1),
        Point(0, 2),
        // Point(1, 2),
        Point(2, 2)
      )

    val path: Batch[Point] =
      PathFinder.locatePath(start, end, walkable)

    assertEquals(path, Batch.empty)
  }

  test("Destination is invalid") {
    val start: Point = Point(0, 0)
    val end: Point   = Point(50, 15)

    val walkable: Batch[Point] =
      Batch(
        Point(0, 0),
        Point(1, 0),
        Point(2, 0),
        Point(0, 1),
        Point(1, 1),
        Point(2, 1),
        Point(0, 2),
        Point(1, 2),
        Point(2, 2)
      )

    val path: Batch[Point] =
      PathFinder.locatePath(start, end, walkable)

    assertEquals(path, Batch.empty)
  }

  test("Real path") {
    val start: Point = Point(20, 23) - Point(17, 21)
    val end: Point   = Point(19, 26) - Point(17, 21)
    val walkable: Batch[Point] =
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

    val actual: Batch[Point] =
      PathFinder.locatePath(start, end, walkable)

    val possiblePaths: List[List[Point]] = List(
      List(Point(3, 2), Point(3, 3), Point(3, 4), Point(2, 4), Point(2, 5)),
      List(Point(3, 2), Point(2, 2), Point(2, 3), Point(2, 4), Point(2, 5))
    )

    assert(clue(possiblePaths.contains(actual.toList)))
  }

}
