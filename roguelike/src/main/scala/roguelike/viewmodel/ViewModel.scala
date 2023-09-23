package roguelike.viewmodel

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap
import io.indigoengine.roguelike.starterkit.*
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.NewGame
import roguelike.RogueLikeGame
import roguelike.assets.GameAssets
import roguelike.components.entities.HostilesManager
import roguelike.components.entities.PlayerComponent
import roguelike.components.windows.WindowManager
import roguelike.model.GameMap
import roguelike.model.GameTile
import roguelike.model.Model
import roguelike.model.entity.Collectable
import roguelike.model.entity.Hostile
import roguelike.model.entity.Player
import roguelike.model.gamedata.KeyMapping
import roguelike.subsystems.FloatingMessage
import roguelike.viewmodel.ui.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

final case class ViewModel(game: GameViewModel, mainMenu: MainMenuUi)

object ViewModel:

  def initial(player: Player, initialViewportSize: Size): ViewModel =
    ViewModel(
      GameViewModel.initial(player, initialViewportSize),
      MainMenuUi(Batch(NewGame), Batch.empty, false)
    )

final case class GameViewModel(
    magnification: Int,
    viewportSize: Size,
    squareSize: Point,
    visibleGridSize: Size,
    playerPosition: ActorPosition,
    hostilePositions: Map[Int, ActorPosition],
    lookAtPosition: Point,
    hoverSquare: Point,
    tiles: Batch[(GameTile, Point)],
    tilePositions: Batch[Point],
    collectables: js.Array[Collectable],
    terminals: CachedTerminals,
    helpControlsText: String,
    sprites: Option[GameSprites]
):

  def update(
      context: SceneContext[Size],
      model: Model
  ): GlobalEvent => Outcome[GameViewModel] =
    case KeyboardEvent.KeyDown(KeyMapping.ZoomIn1) | KeyboardEvent.KeyDown(KeyMapping.ZoomIn2) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification + 1)))
      )

    case KeyboardEvent.KeyDown(KeyMapping.ZoomOut1) | KeyboardEvent.KeyDown(KeyMapping.ZoomOut2) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification - 1)))
      )

    case FrameTick if context.mouse.scrolled.exists(_ == MouseWheel.ScrollUp) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification + 1)))
      )

    case FrameTick if context.mouse.scrolled.exists(_ == MouseWheel.ScrollDown) =>
      Outcome(
        this.copy(magnification = Math.min(3, Math.max(1, magnification - 1)))
      )

    case ViewportResize(vp) =>
      Outcome(
        this.copy(
          viewportSize = vp.size
        )
      )

    case FrameTick =>
      GameViewModel.nextViewModel(context, model, this)

    case _: MouseEvent.Click
        if !WindowManager.showingWindow(model) &&
          model.player.position == hoverSquare &&
          model.gameState.isWaitForInput =>
      Outcome(this)
        .addGlobalEvents(GameEvent.PlayerTryPickUp)

    case _: MouseEvent.Click
        if !WindowManager.showingWindow(model) &&
          model.gameState.isWaitForInput =>
      Outcome(this)
        .addGlobalEvents(GameEvent.PlayerMoveTowards(hoverSquare))

    case e: GameEvent =>
      updateGameEvent(context, model)(e)

    case _ =>
      Outcome(this)

  def updateGameEvent(
      context: SceneContext[Size],
      model: Model
  ): GameEvent => Outcome[GameViewModel] =
    case GameEvent.Inventory(InventoryEvent.PickedUp(item)) =>
      Outcome(this)
        .addGlobalEvents(
          FloatingMessage.spawnEvent(
            playerPosition.moving(squareSize),
            FloatingMessage.Message("+1 " + item.name, RGBA.Green)
          )
        )

    case GameEvent.Inventory(InventoryEvent.DropItem(item, _)) =>
      Outcome(this)
        .addGlobalEvents(
          FloatingMessage.spawnEvent(
            playerPosition.moving(squareSize),
            FloatingMessage.Message("-1 " + item.name, RGBA.Red)
          )
        )

    case GameEvent.RedrawHistoryLog =>
      val history =
        model.messageLog
          .toTerminal(Size(50, 30), false, 0, true)
          .toCloneTiles(Point(3, 4), RoguelikeTiles.Size10x10.charCrops) { (fg, bg) =>
            Graphic(10, 10, TerminalText(GameAssets.assets.init.AnikkiSquare10x10, fg, bg))
          }

      val shortLog =
        model.messageLog
          .toTerminal(Size(50, 5), false, 0, false)
          .toCloneTiles(Point(3, 4), RoguelikeTiles.Size10x10.charCrops) { (fg, bg) =>
            Graphic(10, 10, TerminalText(GameAssets.assets.init.AnikkiSquare10x10, fg, bg))
          }

      Outcome(
        this.copy(
          terminals = terminals.copy(history = history, shortLog = shortLog)
        )
      )

    case GameEvent.CameraSnapToPlayer =>
      Outcome(
        this.copy(
          playerPosition = ActorPosition(model.player.position)
        )
      )

    case GameEvent.NPCMoveComplete =>
      HostilesManager.updateViewModel(
        context,
        model,
        this,
        HostilesManager.Cmds.CompleteInProgressHostile
      )

    case GameEvent.PlayerTurnEnd =>
      Outcome(this)

    case GameEvent.PlayerAttack(_, _, _) =>
      Outcome(this)

    case GameEvent.PlayerCastsConfusion(_, _, _) =>
      Outcome(this)

    case GameEvent.PlayerCastsFireball(_, _, _) =>
      Outcome(this)

    case GameEvent.PlayerMoveTowards(_) =>
      Outcome(this)

    case GameEvent.PlayerContinueMove =>
      Outcome(this)

    case GameEvent.PlayerTryPickUp =>
      Outcome(this)

    case GameEvent.PlayerMoveComplete =>
      Outcome(this)

    case GameEvent.Inventory(InventoryEvent.UseConsumables(_)) =>
      Outcome(this)

    case GameEvent.Inventory(InventoryEvent.RemoveFromInventory(_)) =>
      Outcome(this)

    case GameEvent.Inventory(InventoryEvent.UseRanged(_, _)) =>
      Outcome(this)

    case GameEvent.Inventory(InventoryEvent.TargetUsingRanged(_, _)) =>
      Outcome(this)
    case GameEvent.NPCTurnComplete =>
      Outcome(this)

    case GameEvent.Hostile(_) =>
      Outcome(this)

    case GameEvent.Log(_) =>
      Outcome(this)

    case GameEvent.Targeted(_) =>
      Outcome(this)

    case GameEvent.WindowEvent(_) =>
      Outcome(this)

    case GameEvent.PlayerDescended =>
      Outcome(reset)

  def reset: GameViewModel =
    this.copy(
      hostilePositions = Map()
    )

object GameViewModel:

  val SquareSize: Point = Point(32)

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
      magnification = 2,
      viewportSize = initialViewportSize,
      squareSize = SquareSize,
      visibleGridSize = initialViewportSize / SquareSize.toSize,
      playerPosition = ActorPosition(player.position),
      hostilePositions = Map(),
      lookAtPosition = Point.zero,
      hoverSquare = Point.zero,
      tiles = Batch.empty,
      tilePositions = Batch.empty,
      collectables = js.Array(),
      terminals = CachedTerminals.initial,
      helpControlsText = helpControlsText,
      sprites = None
    )

  def nextViewModel(
      context: SceneContext[Size],
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

    val nextPlayerPosition =
      PlayerComponent.updateViewModel(
        context,
        model,
        viewModel,
        PlayerComponent.Cmds.Update
      )

    val hostilesPositions =
      HostilesManager.updateViewModel(
        context,
        model,
        viewModel,
        HostilesManager.Cmds.Update(model.gameMap)
      )

    (nextPlayerPosition combine hostilesPositions).map { case (pp, hps) =>
      viewModel.copy(
        visibleGridSize = visibleMapSize,
        playerPosition = pp.playerPosition,
        hostilePositions = hps.hostilePositions,
        lookAtPosition = (model.lookAtTarget * viewModel.squareSize) - (viewModel.squareSize.x / 2),
        hoverSquare = hoverSquare,
        tiles = tiles,
        tilePositions = tiles.map(_._2),
        collectables = collectables
      )
    }

final case class CachedTerminals(
    history: TerminalClones,
    shortLog: TerminalClones
)

object CachedTerminals:
  def initial: CachedTerminals =
    CachedTerminals(TerminalClones.empty, TerminalClones.empty)

final case class GameSprites(player: Sprite[Bitmap], orc: Sprite[Bitmap], troll: Sprite[Bitmap])
