package dungeonviewer

import dungeongen.classic.DungeonRules
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

      Outcome(viewModel.copy(terminal = terminal))

    case _ =>
      Outcome(viewModel)

  def drawTriangle(t: Triangle): Shape.Polygon =
    Shape.Polygon(t.closed.map(_.toPoint), Fill.None, Stroke(1, RGBA.Magenta))

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

    val points: Batch[Shape.Circle] =
      model.points.map { pt =>
        Shape.Circle(
          pt,
          5,
          Fill.Color(RGBA.Green)
        )
      }

    val superTriangle = Batch(drawTriangle(model.superTriangle))

    Outcome(
      SceneUpdateFragment(
        tiles.clones ++ points ++ superTriangle
      )
        .addCloneBlanks(tiles.blanks)
    )

final case class ViewModel(terminal: TerminalEmulator)
object ViewModel:
  val initial: ViewModel =

    val terminal: TerminalEmulator =
      TerminalEmulator(Size(80, 50))
        .putLine(Point(1, 1), "Hit the space key to generate / regenerate", RGBA.White, RGBA.Black)

    ViewModel(terminal)

final case class Model(points: Batch[Point], superTriangle: Triangle)
object Model:

  def randomPoint(dice: Dice, offset: Point): Point =
    Point(dice.rollFromZero(300), dice.rollFromZero(300)).moveBy(offset)

  val initial: Model =
    val dice          = Dice.fromSeed(1)
    val offset        = Point(150, 200)
    val points        = List.fill(5)(randomPoint(dice, offset)).toBatch
    val superTriangle = Triangle.encompassing(points.map(_.toVertex))

    Model(
      points,
      superTriangle
    )

final case class Triangle(a: Vertex, b: Vertex, c: Vertex):
  val vertices: Batch[Vertex] = Batch(a, b, c)
  val closed: Batch[Vertex]   = Batch(a, b, c, a)

object Triangle:

  // A triangle big enough to comfortably contain a point cloud
  // Some fudged numbers here.
  def encompassing(pointCloud: Batch[Vertex]): Triangle =
    encompassing(BoundingBox.fromVertexCloud(pointCloud).expand(20))

  def encompassing(b: BoundingBox): Triangle =
    val t = Vertex(b.center.x, b.center.y - b.height)
    val l = Vertex(b.center.x - b.width, b.bottom)
    val r = Vertex(b.center.x + b.width, b.bottom)

    Triangle(t, l, r)
