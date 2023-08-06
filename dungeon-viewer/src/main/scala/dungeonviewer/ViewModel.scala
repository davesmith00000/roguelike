package dungeonviewer

import dungeongen.v2.Mesh
import dungeongen.v2.Triangle
import indigo.*
import indigo.syntax.*
import io.indigoengine.roguelike.starterkit.*
import io.indigoengine.roguelike.starterkit.terminal.MapTile
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.Dungeon
import roguelike.model.entity.Hostile
import roguelike.model.entity.Orc
import roguelike.model.entity.Troll

import scala.annotation.tailrec

final case class ViewModel(
    terminal: TerminalEmulator,
    dungeonMesh: Batch[LineSegment],
    doorwayLines: Batch[LineSegment],
    roomPoints: Batch[Point],
    roomSuperTriangle: Triangle,
    isMeshVisible: Boolean
)
object ViewModel:
  def addInstructions(t: TerminalEmulator): TerminalEmulator =
    t
      .putLine(
        Point(1, 1),
        "Hit the space key to generate / regenerate (v2).",
        RGBA.White,
        RGBA.Black
      )
      .putLine(
        Point(1, 2),
        "or press '1' for a classic, or '2' for a v2 map.",
        RGBA.White,
        RGBA.Black
      )
      .putLine(
        Point(1, 3),
        "Press 'h' to show / hide the v2 mesh.",
        RGBA.White,
        RGBA.Black
      )

  val initial: ViewModel =
    val terminal: TerminalEmulator =
      addInstructions(TerminalEmulator(Size(80, 50)))

    ViewModel(
      terminal,
      Batch.empty,
      Batch.empty,
      Batch.empty,
      Triangle(Vertex(-1), Vertex(-1), Vertex(-1)),
      true
    )

  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  def updateFromDungeon(dice: Dice, viewModel: ViewModel, dungeon: Dungeon): Outcome[ViewModel] =
    val bg: Batch[(Point, MapTile)] =
      dungeon.positionedTiles.toBatch.map { p =>
        p.tile match
          case Wall(code) =>
            p.position -> MapTile(Tile.`▓`, RGBA.Crimson)

          case Ground(style) =>
            val c =
              if style % 2 == 0 then RGBA.Red.mix(RGBA.Black, 0.4)
              else RGBA.Red.mix(RGBA.Black, 0.75)
            p.position -> MapTile(Tile.`░`, c)

          case DownStairs =>
            p.position -> MapTile(Tile.BLACK_UP_POINTING_TRIANGLE, RGBA.Yellow)
      }

    val hostiles: Batch[(Point, MapTile)] =
      dungeon.hostiles.toBatch.map {
        case h: Orc =>
          h.position -> MapTile(Tile.`☺`, RGBA.Green)

        case h: Troll =>
          h.position -> MapTile(Tile.`☺`, RGBA.Olive)
      }

    val player: Batch[(Point, MapTile)] =
      Batch(dungeon.playerStart -> MapTile(Tile.`@`, RGBA.White))

    val tiles: Batch[(Point, MapTile)] =
      bg ++ hostiles ++ player

    val terminal: TerminalEmulator =
      addInstructions(
        TerminalEmulator(Size(80, 50))
          .put(tiles)
      )

    // TODO: This is all being done in the wrong place, should be part of the map builder, just noodling.

    val roomsAsBounds = dungeon.meta.rooms.toBatch.map(_.toBoundingBox)
    val roomPoints    = dungeon.meta.rooms.toBatch.map(r => r.center)
    val vertices      = roomPoints.map(_.toVertex)
    val superTriangle = Triangle.encompassing(vertices)

    val mesh = Mesh.fromVertices(vertices, superTriangle)

    val goodLines = mesh.toLineSegments.filter {
      case ls @ LineSegment(start, end)
          if roomsAsBounds
            .exists(bb => !bb.contains(start) && !bb.contains(end) && bb.lineIntersects(ls)) =>
        false

      case _ =>
        true
    }

    @tailrec
    def rec(
        remainingRooms: Batch[BoundingBox],
        remainingLines: Batch[LineSegment],
        acc: Batch[LineSegment]
    ): Batch[LineSegment] =
      remainingRooms match
        case Batch() =>
          acc

        case _ if remainingLines.isEmpty =>
          acc

        case r ==: rs =>
          val (connected, notConnected) =
            remainingLines.partition(ls => r.contains(ls.start) || r.contains(ls.end))

          val res1 =
            connected
              .filter(ls => rs.exists(_.contains(ls.start)) || rs.exists(_.contains(ls.end)))
              .sortBy(_.length)
              .splitAt(dice.roll(2))
              ._1

          val res2 = res1.headOption match
            case None =>
              Batch.empty

            case Some(shortest) =>
              val l = shortest.length
              res1.filter(_.length <= l * 1.25)

          rec(rs, notConnected, res2 ++ acc)

        case _ =>
          throw new Exception("Pattern matching error on Batch of room bounds.")

    val connections = rec(roomsAsBounds, goodLines, Batch.empty)

    val roomsActualSize = roomsAsBounds.map(_.contract(2.0))

    val doorwayLines = connections.flatMap { ls =>
      val res =
        for {
          roomA <- roomsActualSize.find(bb => bb.contains(ls.start))
          roomB <- roomsActualSize.find(bb => bb.contains(ls.end))
          start <- roomA.lineIntersectsAt(ls)
          end   <- roomB.lineIntersectsAt(ls)
        } yield makeRoomLine(dice, roomA, roomB, start, end)

      res.toBatch
    }

    Outcome(
      viewModel.copy(
        terminal = terminal,
        roomPoints = roomPoints.map(_ * Point(10)),
        roomSuperTriangle = Triangle(
          superTriangle.a * Vertex(10),
          superTriangle.b * Vertex(10),
          superTriangle.c * Vertex(10)
        ),
        dungeonMesh = connections.map(ls =>
          ls.moveStartTo(ls.start * Vertex(10)).moveEndTo(ls.end * Vertex(10))
        ),
        doorwayLines = doorwayLines
      )
    )

  // Workout the doorway lines.
  def makeRoomLine(
      dice: Dice,
      roomA: BoundingBox,
      roomB: BoundingBox,
      start: Vertex,
      end: Vertex
  ): LineSegment =
    // TODO: We want to adjust the start and end here. What are the rules? Always from A to B.

    // Can we make a perfectly straight line vertical line within bounds?
    def verticalHit =
      dice
        .shuffle((0 to (roomA.width.toInt)).toList)
        .map { i =>
          val line = LineSegment(
            Vertex(roomA.position.x + i, roomA.center.y),
            Vertex(roomA.position.x + i, roomB.center.y)
          )
          roomB.lineIntersectsAt(line)
        }
        .collectFirst { case Some(doorwayPosition) => doorwayPosition }

    // Can we make a perfectly straight line horizontal line within bounds?
    def horizontalHit =
      dice
        .shuffle((0 to (roomA.height.toInt)).toList)
        .map { i =>
          val line = LineSegment(
            Vertex(roomA.center.x, roomA.position.y + i),
            Vertex(roomB.center.x, roomA.position.y + i)
          )
          roomB.lineIntersectsAt(line)
        }
        .collectFirst { case Some(doorwayPosition) => doorwayPosition }

    // If we had context... could we reuse doorways already created?

    verticalHit match
      case None =>
        horizontalHit match
          case None =>
            // standard placement is an ok fall back plan
            LineSegment(start, end)

          case Some(doorwayPosition) =>
            LineSegment(
              Vertex(roomA.center.x, doorwayPosition.y),
              Vertex(roomB.center.x, doorwayPosition.y)
            )

      case Some(doorwayPosition) =>
        LineSegment(
          Vertex(doorwayPosition.x, roomA.center.y),
          Vertex(doorwayPosition.x, roomB.center.y)
        )
