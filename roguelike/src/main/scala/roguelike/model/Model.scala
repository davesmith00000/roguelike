package roguelike.model

import indigo.*
import indigo.scenes.SceneContext
import indigo.syntax.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.GenerateLevel
import roguelike.HostileEvent
import roguelike.InventoryEvent
import roguelike.RogueLikeGame
import roguelike.components.entities.HostilesManager
import roguelike.components.entities.PlayerComponent
import roguelike.components.windows.ActiveWindow
import roguelike.components.windows.WindowManager
import roguelike.components.windows.WindowManagerCommand
import roguelike.model.dungeon.Dungeon
import roguelike.model.entity.*
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

final case class Model( // TODO: Should there be a GameModel class too? (Similar to GameViewModel?)
    player: Player,
    stairsPosition: Point, // TODO: Move into game map?
    lookAtTarget: Point,
    gameMap: GameMap,
    messageLog: MessageLog,
    gameState: GameState,
    targetingWithRangedAt: Option[
      (Ranged, Int)
    ], // TODO - is this really an 'active' inventory slot?
    loadInfo: GameLoadInfo,
    currentFloor: Int, // TODO: Should live where? Is there a 'level' metadata section missing?
    autoMovePath: Batch[Point],
    windowManager: ActiveWindow,
    collectables: Batch[Collectable],
    hostiles: HostilesPool
):
  def entitiesList: js.Array[Entity] =
    (collectables.toJSArray ++
      hostiles.toJSArray.sortBy(_.isAlive))
      .filter(e => gameMap.visible.contains(e.position) && e.position != stairsPosition) :+ player

  def closeAllWindows: Model =
    this.copy(
      gameState =
        if gameState == GameState.ShowingWindow then GameState.WaitForInput
        else gameState
    )

  def pauseForWindow: Model =
    this.copy(
      gameState = GameState.ShowingWindow
    )

  def toggleLookAround(radius: Int): Model =
    val show = !gameState.lookingAround
    this.copy(
      gameState = if show then GameState.LookAround(radius) else GameState.WaitForInput,
      lookAtTarget = player.position
    )

  def handleInventoryEvent(
      context: SceneContext[Size]
  ): InventoryEvent => Outcome[Model] = {
    case InventoryEvent.RemoveFromInventory(at) =>
      PlayerComponent.updateModel(
        context,
        this,
        PlayerComponent.Cmds.RemoveFromInvetory(at)
      )

    case InventoryEvent.UseConsumables(c) =>
      PlayerComponent
        .updateModel(
          context,
          this,
          PlayerComponent.Cmds.UseConsumable(c)
        )

    case InventoryEvent.DropItem(item, mapPosition) =>
      Outcome(
        this.copy(
          collectables = Collectable(mapPosition, item) :: collectables
        )
      )

    case InventoryEvent.PickedUp(_) =>
      Outcome(this)

    case InventoryEvent.UseRanged(inventoryPosition, Ranged.LightningScroll) =>
      RangedHelper
        .useLightningScroll(
          player,
          hostiles
            .findClosest(player.position, Ranged.LightningScroll.maxRange + 1)
        )
        .createGlobalEvents { consumed =>
          val maybeRemove =
            if consumed then
              Batch(
                GameEvent.Inventory(
                  InventoryEvent
                    .RemoveFromInventory(inventoryPosition)
                )
              )
            else Batch.empty

          maybeRemove ++ Batch(GameEvent.PlayerTurnEnd)
        }
        .map(_ => this)

    case InventoryEvent.UseRanged(inventoryPosition, Ranged.ConfusionScroll) =>
      Outcome(this) // Shouldn't happen

    case InventoryEvent.UseRanged(inventoryPosition, Ranged.FireballScroll) =>
      Outcome(this) // Shouldn't happen

    case InventoryEvent.TargetUsingRanged(
          inventoryPosition,
          Ranged.LightningScroll
        ) =>
      Outcome(this) // Shouldn't happen

    case InventoryEvent.TargetUsingRanged(
          inventoryPosition,
          Ranged.ConfusionScroll
        ) =>
      Outcome(
        this.copy(
          gameState = GameState.LookAround(Ranged.ConfusionScroll.radius),
          lookAtTarget = player.position,
          targetingWithRangedAt = Option((Ranged.ConfusionScroll, inventoryPosition))
        )
      )

    case InventoryEvent.TargetUsingRanged(
          inventoryPosition,
          Ranged.FireballScroll
        ) =>
      Outcome(
        this.copy(
          gameState = GameState.LookAround(Ranged.FireballScroll.radius),
          lookAtTarget = player.position,
          targetingWithRangedAt = Option((Ranged.FireballScroll, inventoryPosition))
        )
      )
  }

  def update(context: SceneContext[Size]): GameEvent => Outcome[Model] =
    case GameEvent.PlayerTryPickUp =>
      pickUp

    case GameEvent.WindowEvent(command) =>
      WindowManager.updateModel(context, this, command)

    case GameEvent.Log(message) =>
      Outcome(
        this.copy(
          messageLog = messageLog.addMessage(message)
        ),
        Batch(GameEvent.RedrawHistoryLog)
      )

    case GameEvent.Inventory(e) =>
      handleInventoryEvent(context)(e)

    case GameEvent.Hostile(e) =>
      PlayerComponent.updateModel(
        context,
        this,
        PlayerComponent.Cmds.HostileInteraction(e)
      )

    case GameEvent.Targeted(position) =>
      targetingWithRangedAt match
        case None =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(Message("No item selected", ColorScheme.impossible))
            )

        case Some(_) if position == player.position =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(
                Message("You cannot target yourself!", ColorScheme.impossible)
              )
            )

        case Some(_) if !gameMap.visible.contains(position) =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(
                Message(
                  "You cannot target an area that you cannot see!",
                  ColorScheme.impossible
                )
              )
            )

        case Some(_) if !hostiles.existsAt(position) =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(
                Message(
                  "You must select an enemy to target.",
                  ColorScheme.impossible
                )
              )
            )

        case Some((ranged, inventoryPosition)) =>
          hostiles.findByPosition(position) match
            case None =>
              Outcome(this)
                .addGlobalEvents(
                  GameEvent.Log(
                    Message(
                      "You must select an enemy to target.",
                      ColorScheme.impossible
                    )
                  )
                )

            case Some(target) =>
              ranged match
                case Ranged.LightningScroll =>
                  // Shouldn't happen
                  Outcome(this).addGlobalEvents(
                    GameEvent.Log(
                      Message(
                        "Cannot target with a lightning scroll.",
                        ColorScheme.impossible
                      )
                    )
                  )

                case Ranged.ConfusionScroll =>
                  RangedHelper
                    .useConfusionScroll(player, target)
                    .createGlobalEvents { consumed =>
                      val maybeRemove =
                        if consumed then
                          Batch(
                            GameEvent.Inventory(
                              InventoryEvent
                                .RemoveFromInventory(inventoryPosition)
                            )
                          )
                        else Batch.empty

                      maybeRemove ++ Batch(GameEvent.PlayerTurnEnd)
                    }
                    .map(_ => this)

                case Ranged.FireballScroll =>
                  RangedHelper
                    .useFireballScroll(
                      player,
                      hostiles.findAllInRange(
                        target.position,
                        Ranged.FireballScroll.radius
                      )
                    )
                    .createGlobalEvents { consumed =>
                      val maybeRemove =
                        if consumed then
                          Batch(
                            GameEvent.Inventory(
                              InventoryEvent
                                .RemoveFromInventory(inventoryPosition)
                            )
                          )
                        else Batch.empty

                      maybeRemove ++ Batch(GameEvent.PlayerTurnEnd)
                    }
                    .map(_ => this)

    case GameEvent.PlayerAttack(name, power, id) =>
      HostilesManager.updateModel(
        context,
        this,
        HostilesManager.Cmds.AttackHostileMelee(name, id, power)
      )

    case GameEvent.PlayerCastsConfusion(name, numberOfTurns, id) =>
      HostilesManager.updateModel(
        context,
        this,
        HostilesManager.Cmds.ConfuseHostile(name, id, numberOfTurns)
      )

    case GameEvent.PlayerCastsFireball(name, damage, id) =>
      HostilesManager.updateModel(
        context,
        this,
        HostilesManager.Cmds.AttackHostileRanged(name, id, damage)
      )

    case GameEvent.PlayerTurnEnd =>
      Outcome(this.copy(gameState = GameState.UpdatingPlayer))

    case GameEvent.PlayerMoveTowards(target) =>
      if autoMovePath.isEmpty then
        val p =
          gameMap
            .getPathTo(player.position, target, Batch.empty)
            .drop(1)
        if p.nonEmpty then
          val next      = p.head
          val remaining = p.tail
          performPlayerTurn(context.dice, next - player.position).map { m =>
            if m.player.position == player.position then
              m.copy(
                autoMovePath = Batch.empty
              )
            else
              m.copy(
                autoMovePath = remaining
              )
          }
        else
          Outcome(
            this.copy(
              autoMovePath = Batch.empty
            )
          ).addGlobalEvents(
            GameEvent.Log(
              Message("No valid move found.", ColorScheme.invalid)
            )
          )
      else
        Outcome(
          this.copy(
            autoMovePath = Batch.empty
          )
        ).addGlobalEvents(
          GameEvent.Log(
            Message("Cancelling auto-movement.", ColorScheme.white)
          )
        )

    case GameEvent.PlayerContinueMove =>
      if autoMovePath.isEmpty then Outcome(this)
      else
        performPlayerTurn(context.dice, autoMovePath.head - player.position)
          .map {
            _.copy(
              autoMovePath = autoMovePath.drop(1)
            )
          }

    case GameEvent.NPCTurnComplete =>
      val noHostilesVisible: Boolean =
        hostiles.toJSArray
          .filter(e => e.isAlive && gameMap.visible.contains(e.position))
          .isEmpty

      if noHostilesVisible then
        val events =
          if autoMovePath.isEmpty then Batch.empty
          else Batch(GameEvent.PlayerContinueMove)

        Outcome(this.copy(gameState = GameState.WaitForInput))
          .addGlobalEvents(events)
      else
        Outcome(
          this.copy(
            gameState = GameState.WaitForInput,
            autoMovePath = Batch.empty
          )
        )

    case GameEvent.NPCMoveComplete =>
      HostilesManager.updateModel(
        context,
        this,
        HostilesManager.Cmds.CompleteInProgressHostile
      )

    case GameEvent.PlayerDescended =>
      Outcome(this).addGlobalEvents(GenerateLevel)

    case GameEvent.RedrawHistoryLog =>
      Outcome(this)

    case GameEvent.CameraSnapToPlayer =>
      Outcome(this)

    case GameEvent.PlayerMoveComplete =>
      Outcome(gameMap.update(player.position)).map { gm =>
        this.copy(
          gameMap = gm,
          gameState = GameState.UpdateNPCs,
          hostiles = hostiles.queueAll
        )
      }

  // TODO: Move all the player movement stuff to the PlayerComponent.
  def performPlayerTurn(dice: Dice, by: Point): Outcome[Model] =
    player.bump(by, gameMap, hostiles).map(p => this.copy(player = p))
  def moveUp(dice: Dice): Outcome[Model] =
    performPlayerTurn(dice, Point(0, -1))
  def moveDown(dice: Dice): Outcome[Model] =
    performPlayerTurn(dice, Point(0, 1))
  def moveLeft(dice: Dice): Outcome[Model] =
    performPlayerTurn(dice, Point(-1, 0))
  def moveRight(dice: Dice): Outcome[Model] =
    performPlayerTurn(dice, Point(1, 0))

  def performMoveLookAtTarget(by: Point): Outcome[Model] =
    Outcome(this.copy(lookAtTarget = lookAtTarget + by))

  def lookUp: Outcome[Model]    = performMoveLookAtTarget(Point(0, -1))
  def lookDown: Outcome[Model]  = performMoveLookAtTarget(Point(0, 1))
  def lookLeft: Outcome[Model]  = performMoveLookAtTarget(Point(-1, 0))
  def lookRight: Outcome[Model] = performMoveLookAtTarget(Point(1, 0))

  def pickUp: Outcome[Model] =
    player.pickUp(collectables).map { case (p, updatedCollectables) =>
      this.copy(
        player = p,
        collectables = updatedCollectables
      )
    }

  def toSaveData: ModelSaveData =
    ModelSaveData(
      player,
      stairsPosition,
      gameMap,
      messageLog,
      currentFloor,
      collectables.toList,
      hostiles.hostiles.toList
    )

object Model:

  val HistoryWindowSize: Size   = Size(50, 36)
  val InventoryWindowSize: Size = Size(30, 10)
  val DropWindowSize: Size      = Size(30, 10)
  val QuitWindowSize: Size      = Size(30, 10)

  def blank(dice: Dice): Model =
    val p = Player.initial(dice, Point.zero)
    Model(
      p,
      Point.zero,
      Point.zero,
      GameMap.initial(RogueLikeGame.screenSize),
      MessageLog.DefaultLimited,
      GameState.WaitForInput,
      None,
      GameLoadInfo.initial,
      0,
      Batch.empty,
      WindowManager.initialModel,
      Batch.empty,
      HostilesPool(Batch.empty)
    )

  def fromSaveData(model: Model, saveData: ModelSaveData): Model =
    // Can use Dice.fromSeed(0) here since player is overwritten with saved data.
    blank(Dice.fromSeed(0)).copy(
      player = saveData.player,
      stairsPosition = saveData.stairsPosition,
      gameMap = saveData.gameMap,
      messageLog = saveData.messageLog,
      loadInfo = model.loadInfo.withSaveData(saveData),
      currentFloor = saveData.currentFloor,
      gameState = GameState.UpdatingPlayer,
      collectables = saveData.collectables.toBatch,
      hostiles = HostilesPool(saveData.hostiles.toBatch)
    )

  def assignDungeon(
      dice: Dice,
      dungeon: Dungeon,
      gameMap: GameMap
  ): Outcome[Model] = assignDungeon(dice, dungeon, gameMap, None)

  def assignDungeon(
      dice: Dice,
      dungeon: Dungeon,
      gameMap: GameMap,
      model: Option[Model]
  ): Outcome[Model] =
    Outcome(gameMap)
      .map { gm =>
        model match {
          case Some(currentModel) =>
            currentModel.copy(
              player = currentModel.player.copy(position = dungeon.playerStart),
              stairsPosition = dungeon.stairsPosition,
              gameMap = gm,
              currentFloor = dungeon.currentFloor,
              hostiles = HostilesPool(Batch.fromList(dungeon.hostiles))
            )
          case None =>
            val p = Player.initial(dice, dungeon.playerStart)
            Model(
              p,
              dungeon.stairsPosition,
              Point.zero,
              gm,
              MessageLog.DefaultLimited,
              GameState.WaitForInput,
              None,
              GameLoadInfo.initial,
              dungeon.currentFloor,
              Batch.empty,
              WindowManager.initialModel,
              Batch.fromList(dungeon.collectables),
              HostilesPool(Batch.fromList(dungeon.hostiles))
            )
        }
      }
