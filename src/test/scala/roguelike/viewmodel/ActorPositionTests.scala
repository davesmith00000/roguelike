package roguelike.viewmodel

import indigo.*

class ActorPositionTests extends munit.FunSuite {

  test("does not move if arrived at target") {
    val actorPosition =
      ActorPosition(Point(10), Point(10), Vector2(10), Seconds.zero, Point(24), Seconds(1))

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(1), Seconds(1))

    val expected =
      Vector2(10)

    assert(clue(actual) ~== clue(expected))
  }

  test("can calculate the next position toward target (positive)") {
    val current    = Point(0)
    val target     = Point(24)
    val precise    = Vector2(0)
    val squareSize = Point(24)
    val actorPosition =
      ActorPosition(current, target, precise, Seconds.zero, squareSize, Seconds(1))

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(12)

    assert(clue(actual) ~== clue(expected))
  }

  test("can calculate the next position toward target (positive horiztonal)") {
    val current    = Point(0, 24)
    val target     = Point(24, 24)
    val precise    = Vector2(0, 24)
    val squareSize = Point(24)
    val actorPosition =
      ActorPosition(current, target, precise, Seconds.zero, squareSize, Seconds(1))

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(12, 24)

    assert(clue(actual) ~== clue(expected))
  }

  test("can calculate the next position toward target (negative)") {
    val current    = Point(48)
    val target     = Point(24)
    val precise    = Vector2(48)
    val squareSize = Point(24)
    val actorPosition =
      ActorPosition(current, target, precise, Seconds.zero, squareSize, Seconds(1))

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(24 + 12)

    assert(clue(actual) ~== clue(expected))
  }

  test("if the next move would overshoot the target, lands on the target") {
    val current    = Point(0, 0)
    val target     = Point(5, 5)
    val precise    = Vector2(3.1, 4.2)
    val squareSize = Point(24)
    val actorPosition =
      ActorPosition(current, target, precise, Seconds.zero, squareSize, Seconds(1))

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(5)

    assert(clue(actual) ~== clue(expected))
  }

  test("next model") {

    val squareSize      = Point(24)
    val actorPosition   = ActorPosition(Point(0, 0), squareSize, Seconds(1))

    val actual =
      actorPosition
        .next(
          timeDelta = Seconds(0.5),
          gridDestination = Point(1, 1),
          onCompleteEvent = CompleteEvent
        )
        .unsafeGet

    val expected =
      ActorPosition(
        Point(12), // Center of 0,0 is 12x12px
        Point(24),
        Vector2(24),
        Seconds.zero,
        squareSize,
        Seconds(1)
      )

    println(actual)
    println(expected)

    assertEquals(actual, expected)
  }

  case object CompleteEvent extends GlobalEvent

  test("modelToMapSpace") {
    assertEquals(ActorPosition.modelToMapSpaceTileCentered(Point(0), Point(24)), Point(12))
    assertEquals(ActorPosition.modelToMapSpaceTileCentered(Point(1), Point(24)), Point(24 + 12))
    assertEquals(ActorPosition.modelToMapSpaceTileCentered(Point(2, 1), Point(24)), Point(48, 24) + Point(12))
  }

}
