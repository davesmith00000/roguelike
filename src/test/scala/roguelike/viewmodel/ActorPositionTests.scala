package roguelike.viewmodel

import indigo.*

class ActorPositionTests extends munit.FunSuite {

  test("does not move if arrived at target") {
    val actorPosition =
      ActorPosition(Point(10), Point(10), Vector2(10), Seconds.zero)

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(1), Seconds(1))

    val expected =
      Vector2(10)

    assert(clue(actual) ~== clue(expected))
  }

  test("can calculate the next position toward target (positive)") {
    val current       = Point(0)
    val target        = Point(24)
    val precise       = Vector2(0)
    val actorPosition = ActorPosition(current, target, precise, Seconds.zero)

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(12)

    assert(clue(actual) ~== clue(expected))
  }

  test("can calculate the next position toward target (positive horiztonal)") {
    val current       = Point(0, 24)
    val target        = Point(24, 24)
    val precise       = Vector2(0, 24)
    val actorPosition = ActorPosition(current, target, precise, Seconds.zero)

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(12, 24)

    assert(clue(actual) ~== clue(expected))
  }

  test("can calculate the next position toward target (negative)") {
    val current       = Point(48)
    val target        = Point(24)
    val precise       = Vector2(48)
    val actorPosition = ActorPosition(current, target, precise, Seconds.zero)

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(24 + 12)

    assert(clue(actual) ~== clue(expected))
  }

  test("if the next move would overshoot the target, lands on the target") {
    val current       = Point(0, 0)
    val target        = Point(5, 5)
    val precise       = Vector2(3.1, 4.2)
    val actorPosition = ActorPosition(current, target, precise, Seconds.zero)

    val actual =
      ActorPosition.moveTowardsTarget(actorPosition, Seconds(0.5), Seconds(1))

    val expected =
      Vector2(5)

    assert(clue(actual) ~== clue(expected))
  }

}
