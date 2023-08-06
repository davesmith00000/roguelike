package dungeonviewer

import dungeongen.classic.DungeonRules
import dungeongen.v2.Mesh
import dungeongen.v2.Triangle
import indigo.*
import indigo.scenes.*
import indigo.shared.geometry.LineSegment
import indigo.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.model.dungeon.Dungeon

import scala.annotation.tailrec

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
    case KeyboardEvent.KeyUp(_) =>
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
    case KeyboardEvent.KeyUp(Key.SPACE) | KeyboardEvent.KeyUp(Key.KEY_2) =>
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

      ViewModel.updateFromDungeon(context.dice, viewModel, dungeon)

    case KeyboardEvent.KeyUp(Key.KEY_1) =>
      val dungeon: Dungeon =
        dungeongen.classic.DungeonGen.makeMap(
          context.dice,
          DungeonRules.MaxRooms,
          DungeonRules.RoomMinSize,
          DungeonRules.RoomMaxSize,
          Size(80, 50),
          DungeonRules.maxMonstersPerRoom(0),
          DungeonRules.maxCollectablesPerRoom(0),
          0
        )

      ViewModel.updateFromDungeon(context.dice, viewModel, dungeon)

    case KeyboardEvent.KeyUp(Key.KEY_H) =>
      Outcome(viewModel.copy(isMeshVisible = !viewModel.isMeshVisible))

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

    val mesh: Batch[SceneNode] =
      if viewModel.isMeshVisible then
        drawMesh(model.points, model.superTriangle, model.mesh.toLineSegments) ++
          drawMesh(viewModel.roomPoints, viewModel.roomSuperTriangle, viewModel.dungeonMesh) ++
          viewModel.doorwayLines.flatMap { ls =>
            val start = (ls.start * Vertex(10)).toPoint
            val end   = (ls.end * Vertex(10)).toPoint

            Batch(
              Shape.Line(start, end, Stroke(1, RGBA.Yellow)),
              Shape.Circle(
                start,
                3,
                Fill.Color(RGBA.Yellow)
              ),
              Shape.Circle(
                end,
                3,
                Fill.Color(RGBA.Yellow)
              )
            )
          }
      else Batch.empty

    Outcome(
      SceneUpdateFragment(
        tiles.clones ++ mesh
      )
        .addCloneBlanks(tiles.blanks)
    )

  def drawMesh(
      points: Batch[Point],
      superTriangle: Triangle,
      lines: Batch[LineSegment]
  ): Batch[SceneNode] =
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

    val ml = lines.map { ls =>
      Shape.Line(ls.start.toPoint, ls.end.toPoint, Stroke(1, RGBA.Cyan))
    }

    pts ++ st ++ ml

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
