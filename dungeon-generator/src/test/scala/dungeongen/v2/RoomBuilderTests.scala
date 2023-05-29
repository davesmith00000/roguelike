package dungeongen.v2

import indigo.*

class RoomBuilderTests extends munit.FunSuite:

  test("move a room - no need to move") {

    val actual: Option[Room] =
      RoomBuilder.moveRoom(
        Room(Rectangle(0, 0, 3, 3)),
        Nil,
        Vector2(1, 1),
        Rectangle(0, 0, 100, 100)
      )

    val expected: Room =
      Room(Rectangle(0, 0, 3, 3))

    assert(actual.isDefined)
    assertEquals(actual.get, expected)
  }

  test("move a room - push it along 1") {

    val room = Room(Rectangle(0, 0, 3, 3))

    val existing = List(room)

    val actual: Option[Room] =
      RoomBuilder.moveRoom(
        room,
        existing,
        Vector2(1, 0),
        Rectangle(0, 0, 100, 100)
      )

    val expected: Room =
      Room(Rectangle(3, 0, 3, 3))

    assert(actual.isDefined)
    assertEquals(actual.get, expected)
  }

  test("move a room - push it along 2") {

    val room = Room(Rectangle(0, 0, 3, 3))

    val existing = List(
      room,
      Room(Rectangle(3, 0, 3, 3))
    )

    val actual: Option[Room] =
      RoomBuilder.moveRoom(
        room,
        existing,
        Vector2(1, 0),
        Rectangle(0, 0, 100, 100)
      )

    val expected: Room =
      Room(Rectangle(6, 0, 3, 3))

    assert(actual.isDefined)
    assertEquals(actual.get, expected)
  }

  test("move a room - push it outside the area") {

    val room = Room(Rectangle(0, 0, 3, 3))

    val existing = List(
      room,
      Room(Rectangle(3, 0, 3, 3))
    )

    val actual: Option[Room] =
      RoomBuilder.moveRoom(
        room,
        existing,
        Vector2(1, 0),
        Rectangle(0, 0, 7, 7)
      )

    assert(actual.isEmpty)
  }
