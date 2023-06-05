package dungeongen.v2

import indigo.*

import scala.annotation.tailrec

object RoomBuilder:

  def makeRoom(dice: Dice, center: Point, roomMinSize: Int, roomMaxSize: Int): Room =
    val w = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize
    val h = dice.rollFromZero(roomMaxSize - roomMinSize) + roomMinSize

    Room(
      Rectangle(
        center.x - (w / 2),
        center.y - (h / 2),
        w,
        h
      )
    )

  def moveRooms(dice: Dice, rooms: List[Room], existing: List[Room], area: Rectangle): List[Room] =
    @tailrec
    def rec(remaining: List[Room], acc: List[Room]): List[Room] =
      remaining match
        case Nil =>
          acc

        case r :: rs =>
          val angle     = (Math.PI * 2) * dice.rollDouble
          val direction = Vector2(Math.sin(angle), Math.cos(angle))

          moveRoom(r, acc, direction, area) match
            case None           => rec(rs, acc)
            case Some(nextRoom) => rec(rs, nextRoom :: acc)

    rec(rooms, existing)

  def moveRoom(
      room: Room,
      otherRooms: List[Room],
      direction: Vector2,
      area: Rectangle
  ): Option[Room] =
    @tailrec
    def rec(bounds: BoundingBox, triesLeft: Int): Option[Room] =
      if triesLeft > 0 then
        val next = bounds.moveBy(direction)
        val r    = next.toRectangle

        if otherRooms.isEmpty then Option(room)
        else if !area.encompasses(r) then None
        else if !otherRooms.exists(_.bounds.overlaps(r)) then Option(Room(r))
        else rec(next, triesLeft - 1)
      else None

    rec(BoundingBox.fromRectangle(room.bounds), Math.max(area.width, area.height))
