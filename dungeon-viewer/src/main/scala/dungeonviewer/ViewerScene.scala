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
      SceneUpdateFragment(tiles.clones)
        .addCloneBlanks(tiles.blanks)
    )

final case class Model()
object Model:
  val initial: Model =
    Model()

final case class ViewModel(terminal: TerminalEmulator)
object ViewModel:
  val initial: ViewModel =

    val terminal: TerminalEmulator =
      TerminalEmulator(Size(80, 50))
        .putLine(Point(1, 1), "Hit the space key to generate / regenerate", RGBA.White, RGBA.Black)

    ViewModel(terminal)
