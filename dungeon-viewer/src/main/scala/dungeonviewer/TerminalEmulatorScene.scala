package dungeonviewer

import indigo.*
import indigo.scenes.*
import io.indigoengine.roguelike.starterkit.*

object TerminalEmulatorScene extends Scene[Unit, Unit, Unit]:

  type SceneModel     = Unit
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("TerminalEmulator scene")

  val modelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val viewModelLens: Lens[Unit, Unit] =
    Lens.keepLatest

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(context: SceneContext[Unit], model: Unit): GlobalEvent => Outcome[Unit] =
    case KeyboardEvent.KeyUp(Key.SPACE) =>
      Outcome(model).addGlobalEvents(SceneEvent.JumpTo(CloneTilesScene.name))

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Unit],
      model: Unit,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  // This shouldn't live here really, just keeping it simple for demo purposes.
  val terminal: TerminalEmulator =
    TerminalEmulator(Size(3, 3))
      .put(
        Point(0, 0) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(1, 0) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(2, 0) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(0, 1) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(1, 1) -> MapTile(Tile.`@`, RGBA.Magenta),
        Point(2, 1) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(0, 2) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(1, 2) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue),
        Point(2, 2) -> MapTile(Tile.`░`, RGBA.Cyan, RGBA.Blue)
      )

  val entity =
    terminal.draw(
      Assets.tileMap,
      Size(10, 10),
      MapTile(Tile.SPACE),
      RogueLikeGame.maxTileCount
    )

  def present(
      context: SceneContext[Unit],
      model: Unit,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      SceneUpdateFragment(
        entity
      )
    )
