package dungeonviewer

import dungeongen.classic.DungeonRules
import dungeongen.v2.Mesh
import dungeongen.v2.Triangle
import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.GameTile.DownStairs
import roguelike.model.GameTile.Ground
import roguelike.model.GameTile.Wall
import roguelike.model.dungeon.Dungeon
import roguelike.model.entity.Hostile
import roguelike.model.entity.Orc
import roguelike.model.entity.Troll

object ViewerScene extends Scene[Unit, Model, ViewModel]:

  type SceneModel     = Model
  type SceneViewModel = ViewModel

  val name: SceneName =
    SceneName("viewer scene")

  val modelLens: Lens[Model, Model] =
    Lens.keepLatest

  val viewModelLens: Lens[ViewModel, ViewModel] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: SceneContext[Unit], model: Model): GlobalEvent => Outcome[Model] =
    case KeyboardEvent.KeyUp(Key.SPACE) =>
      // Fudge. When we switch to the dungeon, clear the test mesh.
      Outcome(
        model.copy(
          points = Batch.empty,
          mesh = Mesh.empty,
          superTriangle = Triangle(Vertex(-1), Vertex(-1), Vertex(-1))
        )
      )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): GlobalEvent => Outcome[ViewModel] =
    case KeyboardEvent.KeyUp(Key.SPACE) =>
      val dungeon: Dungeon =
        dungeongen.v2.DungeonGen.makeMap(
          context.dice,
          DungeonRules.MaxRooms,
          DungeonRules.RoomMinSize,
          DungeonRules.RoomMaxSize,
          Size(80, 50),
          DungeonRules.maxMonstersPerRoom(0),
          DungeonRules.maxCollectablesPerRoom(0),
          0
        )

        // dungeongen.classic.DungeonGen.makeMap(
        //   context.dice,
        //   DungeonRules.MaxRooms,
        //   DungeonRules.RoomMinSize,
        //   DungeonRules.RoomMaxSize,
        //   Size(80, 50),
        //   DungeonRules.maxMonstersPerRoom(0),
        //   DungeonRules.maxCollectablesPerRoom(0),
        //   0
        // )

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
        TerminalEmulator(Size(80, 50))
          .put(tiles)

      val roomPoints    = dungeon.meta.rooms.toBatch.map(r => r.center * Point(10))
      val vertices      = roomPoints.map(_.toVertex)
      val superTriangle = Triangle.encompassing(vertices)

      Outcome(
        viewModel.copy(
          terminal = terminal,
          roomPoints = roomPoints,
          roomSuperTriangle = superTriangle,
          dungeonMesh = Mesh.fromVertices(vertices, superTriangle)
        )
      )

    case _ =>
      Outcome(viewModel)

  def present(
      context: SceneContext[Unit],
      model: Model,
      viewModel: ViewModel
  ): Outcome[SceneUpdateFragment] =
    val tiles =
      viewModel.terminal
        .toCloneTiles(Point.zero, RoguelikeTiles.Size10x10.charCrops) { (fg, bg) =>
          Graphic(10, 10, TerminalText(Assets.tileMap, fg, bg))
        }

    Outcome(
      SceneUpdateFragment(
        tiles.clones ++
          drawMesh(model.points, model.superTriangle, model.mesh) ++
          drawMesh(viewModel.roomPoints, viewModel.roomSuperTriangle, viewModel.dungeonMesh)
      )
        .addCloneBlanks(tiles.blanks)
    )

  def drawMesh(points: Batch[Point], superTriangle: Triangle, mesh: Mesh): Batch[SceneNode] =
    val pts: Batch[Shape.Circle] =
      points.map { pt =>
        Shape.Circle(
          pt,
          5,
          Fill.Color(RGBA.Green)
        )
      }

    val st = superTriangle.toLineSegments.map { ls =>
      Shape.Line(ls.start.toPoint, ls.end.toPoint, Stroke(1, RGBA.Magenta))
    }

    val ml = mesh.toLineSegments.map { ls =>
      Shape.Line(ls.start.toPoint, ls.end.toPoint, Stroke(1, RGBA.Cyan))
    }

    pts ++ st ++ ml

final case class ViewModel(
    terminal: TerminalEmulator,
    dungeonMesh: Mesh,
    roomPoints: Batch[Point],
    roomSuperTriangle: Triangle
)
object ViewModel:
  val initial: ViewModel =

    val terminal: TerminalEmulator =
      TerminalEmulator(Size(80, 50))
        .putLine(Point(1, 1), "Hit the space key to generate / regenerate", RGBA.White, RGBA.Black)

    ViewModel(terminal, Mesh.empty, Batch.empty, Triangle(Vertex(-1), Vertex(-1), Vertex(-1)))

final case class Model(points: Batch[Point], superTriangle: Triangle, mesh: Mesh)
object Model:

  def randomPoint(dice: Dice, offset: Point): Point =
    Point(dice.rollFromZero(300), dice.rollFromZero(300)).moveBy(offset)

  val initial: Model =
    val dice          = Dice.fromSeed(1)
    val offset        = Point(150, 200)
    val points        = List.fill(10)(randomPoint(dice, offset)).toBatch
    val superTriangle = Triangle.encompassing(points.map(_.toVertex), 10)
    val mesh          = Mesh.fromVertices(points.map(_.toVertex), superTriangle)

    Model(
      points,
      superTriangle,
      mesh
    )
