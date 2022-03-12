package roguelike.viewmodel

import indigo._
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.GameEvent
import roguelike.RogueLikeGame
import roguelike.model.GameMap
import roguelike.model.GamePhase
import roguelike.model.GameTile
import roguelike.model.Model
import roguelike.model.entity.Collectable
import roguelike.model.entity.Hostile
import roguelike.model.entity.Player
import roguelike.model.gamedata.KeyMapping

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class ViewModel(game: GameViewModel)

object ViewModel:

  def initial(player: Player, initialViewportSize: Size): ViewModel =
    ViewModel(GameViewModel.initial(player, initialViewportSize))

final case class GameViewModel(
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
    terminals: CachedTerminals
):
  def update(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[GameViewModel] =
    case KeyboardEvent.KeyDown(KeyMapping.ZoomIn) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification + 1)))
      )

    case KeyboardEvent.KeyDown(KeyMapping.ZoomOut) =>
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
              Graphic(10, 10, TerminalText(Assets.tileMap, fg, bg))
          }

      Outcome(
        this.copy(
          terminals = terminals.copy(history = history)
        )
      )

    case GameEvent.CameraSnapToPlayer =>
      Outcome(
        this.copy(
          playerPosition = ActorPosition.initial(
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

    case MouseEvent.Click(_) =>
      Outcome(this)
        .addGlobalEvents(GameEvent.PlayerMoveTowards(hoverSquare))

    case _ =>
      Outcome(this)

object GameViewModel:

  val SquareSize: Point = Point(24)

  def initial(player: Player, initialViewportSize: Size): GameViewModel =
    GameViewModel(
      magnification = 2,
      viewportSize = initialViewportSize,
      squareSize = SquareSize,
      visibleGridSize = initialViewportSize / SquareSize.toSize,
      playerPosition = ActorPosition.initial(player.position, SquareSize),
      lookAtPosition = Point.zero,
      hoverSquare = Point.zero,
      tiles = js.Array(),
      tilePositions = js.Array(),
      collectables = js.Array(),
      hostiles = js.Array(),
      terminals = CachedTerminals.initial
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
      model.gameMap.collectables.toJSArray
        .filter(collectable =>
          viewModel.tilePositions.contains(collectable.position) &&
            model.gameMap.visible.contains(collectable.position)
        )

    val hostiles =
      model.gameMap.visibleSortedHostiles
        .filter(h => viewModel.tilePositions.contains(h.position))

    val nextPlayerPosition =
      model.gamePhase match
        case GamePhase.PlayerTurn =>
          viewModel.playerPosition
            .next(context.delta, model.player.position, viewModel.squareSize)

        case _ =>
          Outcome(viewModel.playerPosition)

    nextPlayerPosition.map { pp =>
      viewModel.copy(
        visibleGridSize = visibleMapSize,
        playerPosition = pp,
        lookAtPosition =
          (model.lookAtTarget * viewModel.squareSize) + (viewModel.squareSize.x / 2),
        hoverSquare = hoverSquare,
        tiles = tiles,
        tilePositions = tiles.map(_._2),
        collectables = collectables,
        hostiles = hostiles
      )
    }

final case class CachedTerminals(history: TerminalClones)

object CachedTerminals:
  def initial: CachedTerminals =
    CachedTerminals(TerminalClones.empty)
