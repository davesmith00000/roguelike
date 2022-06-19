package roguelike.viewmodel

import indigo._
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.RogueLikeGame
import roguelike.ViewModelEvent
import roguelike.components.entities.PlayerComponent
import roguelike.components.windows.WindowManager
import roguelike.game.MiniMap
import roguelike.model.GameMap
import roguelike.model.GamePhase
import roguelike.model.GameTile
import roguelike.model.Model
import roguelike.model.entity.Collectable
import roguelike.model.entity.Hostile
import roguelike.model.entity.Player
import roguelike.model.gamedata.KeyMapping
import roguelike.subsystems.FloatingMessage

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class ViewModel(game: GameViewModel)

object ViewModel:

  def initial(player: Player, initialViewportSize: Size): ViewModel =
    ViewModel(GameViewModel.initial(player, initialViewportSize))

enum GameViewModelPhase:
  case Idle, MovingPlayer

final case class GameViewModel(
    phase: GameViewModelPhase,
    magnification: Int,
    viewportSize: Size,
    squareSize: Point,
    visibleGridSize: Size,
    playerPosition: ActorPosition,
    lookAtPosition: Point,
    hoverSquare: Point,
    tiles: js.Array[(GameTile, Point)],
    tilePositions: js.Array[Point],
    collectables: js.Array[Collectable],
    hostiles: js.Array[Hostile],
    terminals: CachedTerminals,
    helpControlsText: String,
    miniMap: MiniMap
):
  def update(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[GameViewModel] =
    case GameEvent.Inventory(InventoryEvent.PickedUp(item)) =>
      Outcome(this)
        .addGlobalEvents(
          FloatingMessage.spawnEvent(
            playerPosition.display,
            FloatingMessage.Message("+1 " + item.name, RGB.Green)
          )
        )

    case GameEvent.Inventory(InventoryEvent.DropItem(item, _)) =>
      Outcome(this)
        .addGlobalEvents(
          FloatingMessage.spawnEvent(
            playerPosition.display,
            FloatingMessage.Message("-1 " + item.name, RGB.Red)
          )
        )

    case KeyboardEvent.KeyDown(KeyMapping.ZoomIn1) |
        KeyboardEvent.KeyDown(KeyMapping.ZoomIn2) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification + 1)))
      )

    case KeyboardEvent.KeyDown(KeyMapping.ZoomOut1) |
        KeyboardEvent.KeyDown(KeyMapping.ZoomOut2) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification - 1)))
      )

    case FrameTick if context.mouse.scrolled.exists(_ == MouseWheel.ScrollUp) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification + 1)))
      )

    case FrameTick
        if context.mouse.scrolled.exists(_ == MouseWheel.ScrollDown) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification - 1)))
      )

    case GameEvent.Redraw =>
      Outcome(this)

    case GameEvent.RedrawHistoryLog =>
      val history =
        model.messageLog
          .toTerminal(Size(50, 30), false, 0, true)
          .toCloneTiles(Point(3, 4), RoguelikeTiles.Size10x10.charCrops) {
            (fg, bg) =>
              Graphic(10, 10, TerminalText(Assets.Basic.tileMap, fg, bg))
          }

      Outcome(
        this.copy(
          terminals = terminals.copy(history = history)
        )
      )

    case GameEvent.CameraSnapToPlayer =>
      Outcome(
        this.copy(
          playerPosition = ActorPosition(
            model.player.position,
            GameViewModel.SquareSize
          )
        )
      )

    case ViewportResize(vp) =>
      Outcome(
        this.copy(
          viewportSize = vp.size
        )
      )

    case FrameTick =>
      GameViewModel.nextViewModel(context, model, this)

    case MouseEvent.Click(_) if !WindowManager.showingWindow(model) =>
      Outcome(this)
        .addGlobalEvents(GameEvent.PlayerMoveTowards(hoverSquare))

    case GameEvent.ViewModelHandOff(ViewModelEvent.MovePlayer) =>
      Outcome(this.copy(phase = GameViewModelPhase.MovingPlayer))

    case GameEvent.ViewModelPhaseComplete(e) =>
      Outcome(this.copy(phase = GameViewModelPhase.Idle))
        .addGlobalEvents(GameEvent.ModelHandOff(e))

    case _ =>
      Outcome(this)

object GameViewModel:

  val SquareSize: Point = Point(24)

  def initial(player: Player, initialViewportSize: Size): GameViewModel =
    val separator = " | "
    val commandsAndValues =
      KeyMapping.helpText.map(p => (p._1, p._2.mkString(separator)))
    val longest = KeyMapping.longestMappings + separator.length
    val helpControlsText = commandsAndValues
      .map { case (cmd, value) =>
        val v = List.fill(longest - value.length)(" ").mkString + value
        s"$cmd $v"
      }
      .mkString("\n")

    GameViewModel(
      GameViewModelPhase.Idle,
      magnification = 2,
      viewportSize = initialViewportSize,
      squareSize = SquareSize,
      visibleGridSize = initialViewportSize / SquareSize.toSize,
      playerPosition = ActorPosition(player.position, SquareSize),
      lookAtPosition = Point.zero,
      hoverSquare = Point.zero,
      tiles = js.Array(),
      tilePositions = js.Array(),
      collectables = js.Array(),
      hostiles = js.Array(),
      terminals = CachedTerminals.initial,
      helpControlsText,
      miniMap = MiniMap.initial(Point.zero, RogueLikeGame.screenSize)
    )

  def nextViewModel(
      context: FrameContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[GameViewModel] =
    val hoverSquare =
      val mousePos     = context.mouse.position.toVector
      val squareSize   = viewModel.squareSize.toVector
      val viewportSize = viewModel.viewportSize.toVector
      val partSquare   = (squareSize / 2) * viewModel.magnification
      val offset       = (viewportSize / 2) - partSquare
      val p =
        ((mousePos - offset) / squareSize) / viewModel.magnification

      Point(
        x = if p.x < 0 then p.x.toInt - 1 else p.x.toInt,
        y = if p.y < 0 then p.y.toInt - 1 else p.y.toInt
      ) + model.player.position

    val visibleMapSize =
      (viewModel.viewportSize / viewModel.magnification) / SquareSize.toSize

    val tiles =
      model.gameMap
        .toLocalExplored(model.player.position, visibleMapSize)

    val collectables =
      model.collectables.toJSArray
        .filter(collectable =>
          viewModel.tilePositions.contains(collectable.position) &&
            model.gameMap.visible.contains(collectable.position)
        )

    val hostiles =
      model.gameMap.visibleSortedHostiles
        .filter(h => viewModel.tilePositions.contains(h.position))

    val nextPlayerPosition =
      PlayerComponent.updateViewModel(
        context,
        model,
        viewModel,
        PlayerComponent.Cmds.Update
      )

    val nextMiniMap =
      val gm = model.gameMap
      viewModel.miniMap.update(
        model.player.position,
        gm.exploredWalls
      )

    nextPlayerPosition.map { pp =>
      viewModel.copy(
        visibleGridSize = visibleMapSize,
        playerPosition = pp.playerPosition,
        lookAtPosition =
          (model.lookAtTarget * viewModel.squareSize) + (viewModel.squareSize.x / 2),
        hoverSquare = hoverSquare,
        tiles = tiles,
        tilePositions = tiles.map(_._2),
        collectables = collectables,
        hostiles = hostiles,
        miniMap = nextMiniMap
      )
    }

final case class CachedTerminals(history: TerminalClones)

object CachedTerminals:
  def initial: CachedTerminals =
    CachedTerminals(TerminalClones.empty)
