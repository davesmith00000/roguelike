package roguelike.viewmodel

import indigo.*

class ActorPositionTests extends munit.FunSuite {

  val squareSize: Point = Point(24)

  test("Does not move if arrived at target") {
    val actorPosition =
      ActorPosition(Point(10), Point(10), Seconds.zero, Seconds(1), ActorDirection.Right)

    val actual =
      actorPosition
        .next(Seconds(0.5), Point(10), CompleteEvent)
        .map(_.moving(squareSize))

    val expected =
      Point(10) * squareSize

    assert(clue(actual.unsafeGet) == clue(expected))
  }

  test("Emits event on move completed") {
    val target = Point(1)

    val actorPosition =
      ActorPosition(Point(0), target, Seconds.zero, Seconds(1), ActorDirection.Right)

    val actual =
      actorPosition
        .next(Seconds(1.0), target, CompleteEvent)
        .map(_.moving(squareSize))

    val expected =
      Point(1) * squareSize

    assert(clue(actual.unsafeGet) == clue(expected))
    assert(actual.unsafeGlobalEvents.head == CompleteEvent)
  }

  test("Can calculate the next position toward target (positive)") {
    val current = Point(0)
    val target  = Point(1)
    val actorPosition =
      ActorPosition(current, target, Seconds.zero, Seconds(1), ActorDirection.Right)

    val actual =
      actorPosition
        .next(Seconds(0.5), target, CompleteEvent)
        .map(_.moving(squareSize))
        .unsafeGet

    val expected =
      Point(12, 12)

    assert(clue(actual) == clue(expected))
  }

  test("Can calculate the next position toward target (positive horiztonal)") {
    val current = Point(0, 1)
    val target  = Point(1, 1)
    val actorPosition =
      ActorPosition(current, target, Seconds.zero, Seconds(1), ActorDirection.Right)

    val actual =
      actorPosition
        .next(Seconds(0.5), target, CompleteEvent)
        .map(_.moving(squareSize))
        .unsafeGet

    val expected =
      Point(12, 24)

    assert(clue(actual) == clue(expected))
  }

  test("Can calculate the next position toward target (negative)") {
    val current = Point(2)
    val target  = Point(1)
    val actorPosition =
      ActorPosition(current, target, Seconds.zero, Seconds(1), ActorDirection.Right)

    val actual =
      actorPosition
        .next(Seconds(0.5), target, CompleteEvent)
        .map(_.moving(squareSize))
        .unsafeGet

    val expected =
      Point(24 + 12)

    assert(clue(actual) == clue(expected))
  }

  test("If the next move would overshoot the target, lands on the target") {
    val current = Point(0)
    val target  = Point(5)
    val actorPosition =
      ActorPosition(current, target, Seconds.zero, Seconds(1), ActorDirection.Right)

    val actual =
      actorPosition
        .next(Seconds(1.1), target, CompleteEvent)
        .map(_.moving(squareSize))
        .unsafeGet

    val expected =
      Point(5) * squareSize

    assert(clue(actual) == clue(expected))
  }

  case object CompleteEvent extends GlobalEvent

}
