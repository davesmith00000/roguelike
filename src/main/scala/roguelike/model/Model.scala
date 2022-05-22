package roguelike.model

import indigo._
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.RogueLikeGame
import roguelike.ViewModelEvent
import roguelike.model.entity._
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class Model(
    screenSize: Size,
    player: Player,
    stairsPosition: Point,
    lookAtTarget: Point,
    gameMap: GameMap,
    messageLog: MessageLog,
    paused: Boolean,
    currentState: GameState,
    targetingWithRangedAt: Option[
      (Ranged, Int)
    ], // TODO - is this really an 'active' inventory slot?
    loadInfo: GameLoadInfo,
    currentFloor: Int,
    gamePhase: GamePhase,
    autoMovePath: Batch[Point]
):
  def entitiesList: js.Array[Entity] =
    gameMap.entitiesList
      .filterNot(_.position == stairsPosition) :+ player

  def closeAllWindows: Model =
    this.copy(
      paused = false,
      currentState = GameState.Game
    )

  def toggleMessageHistory: Model =
    val show = !currentState.showingHistory
    this.copy(
      currentState = if show then GameState.History else GameState.Game
    )

  def toggleInventory: Model =
    val show = !currentState.showingInventory
    this.copy(
      currentState = if show then GameState.Inventory else GameState.Game
    )

  def toggleEquipment: Model =
    val show = !currentState.showingEquipment
    this.copy(
      currentState = if show then GameState.Equipment else GameState.Game
    )

  def toggleDropMenu: Model =
    val show = !currentState.showingDropMenu
    this.copy(
      currentState = if show then GameState.Drop else GameState.Game
    )

  def toggleLookAround(radius: Int): Model =
    val show = !currentState.lookingAround
    this.copy(
      currentState =
        if show then GameState.LookAround(radius) else GameState.Game,
      lookAtTarget = player.position
    )

  def toggleQuit: Model =
    val show = !currentState.showingQuit
    this.copy(
      currentState = if show then GameState.Quit else GameState.Game
    )

  def toggleLevelUp: Model =
    val show = !currentState.showingLevelUp
    this.copy(
      currentState = if show then GameState.LevelUp else GameState.Game
    )

  def handleInventoryEvent(dice: Dice): InventoryEvent => Outcome[Model] = {
    case InventoryEvent.UseConsumables(c @ Consumables.HealthPotion) =>
      val possibleAmount  = player.fighter.maxHp - player.fighter.hp
      val amountRecovered = Math.min(possibleAmount, c.amount)

      if amountRecovered <= 0 then
        val msg =
          Message("Your health is already full.", ColorScheme.impossible)
        Outcome(this).addGlobalEvents(
          GameEvent.Log(msg),
          GameEvent.Inventory(InventoryEvent.ReturnConsumablesToInventory(c))
        )
      else
        val msg = Message(
          s"You consume the ${c.name}, and recover $amountRecovered",
          ColorScheme.healthRecovered
        )
        Outcome(this.copy(player = player.heal(amountRecovered)))
          .addGlobalEvents(GameEvent.Log(msg), GameEvent.PlayerTurnEnd)

    case InventoryEvent.ReturnConsumablesToInventory(
          c @ Consumables.HealthPotion
        ) =>
      player
        .take(c)
        .map(p => this.copy(player = p))

    case InventoryEvent.EquipArmour(a: Armour) =>
      player.equip(a).map { p =>
        this.copy(player = p)
      }

    case InventoryEvent.UnequipArmour =>
      player.unequipArmour.map { p =>
        this.copy(player = p)
      }

    case InventoryEvent.EquipMelee(m: Melee) =>
      player.equip(m).map { p =>
        this.copy(player = p)
      }

    case InventoryEvent.UnequipMelee =>
      player.unequipMelee.map { p =>
        this.copy(player = p)
      }

    case InventoryEvent.ReturnArmourToInventory(a: Armour) =>
      val msg =
        Message(s"Returned ${a.name} to your inventory", ColorScheme.white)
      player
        .take(a)
        .map(p => this.copy(player = p))
        .addGlobalEvents(GameEvent.Log(msg), GameEvent.PlayerTurnEnd)

    case InventoryEvent.ReturnMeleeToInventory(m: Melee) =>
      val msg =
        Message(s"Returned ${m.name} to your inventory", ColorScheme.white)
      player
        .take(m)
        .map(p => this.copy(player = p))
        .addGlobalEvents(GameEvent.Log(msg), GameEvent.PlayerTurnEnd)

    case InventoryEvent.DropItem(item, mapPosition) =>
      Outcome(
        this.copy(gameMap =
          gameMap.dropCollectable(Collectable(mapPosition, item))
        )
      )

    case InventoryEvent.PickedUp(_) =>
      Outcome(this)

    case InventoryEvent.UseRanged(inventoryPosition, Ranged.LightningScroll) =>
      RangedHelper
        .useLightningScroll(
          player,
          gameMap.hostiles
            .findClosest(player.position, Ranged.LightningScroll.maxRange + 1)
        )
        .flatMap { consumed =>
          if consumed then
            player.removeInventoryItem(inventoryPosition).map { nextPlayer =>
              this.copy(player = nextPlayer)
            }
          else Outcome(this)
        }
        .addGlobalEvents(GameEvent.PlayerTurnEnd)

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
          currentState = GameState.LookAround(Ranged.ConfusionScroll.radius),
          lookAtTarget = player.position,
          targetingWithRangedAt =
            Option((Ranged.ConfusionScroll, inventoryPosition))
        )
      )

    case InventoryEvent.TargetUsingRanged(
          inventoryPosition,
          Ranged.FireballScroll
        ) =>
      Outcome(
        this.copy(
          currentState = GameState.LookAround(Ranged.FireballScroll.radius),
          lookAtTarget = player.position,
          targetingWithRangedAt =
            Option((Ranged.FireballScroll, inventoryPosition))
        )
      )
  }

  def update(dice: Dice): GameEvent => Outcome[Model] =
    case GameEvent.Log(message) =>
      Outcome(
        this.copy(
          messageLog = messageLog.addMessage(message)
        )
      )

    case GameEvent.Inventory(e) =>
      handleInventoryEvent(dice)(e)

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

        case Some(_) if !gameMap.hostiles.existsAt(position) =>
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
          gameMap.hostiles.findByPosition(position) match
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
                    .flatMap { consumed =>
                      if consumed then
                        player.removeInventoryItem(inventoryPosition).map {
                          nextPlayer =>
                            this.copy(player = nextPlayer).closeAllWindows
                        }
                      else Outcome(this)
                    }
                    .addGlobalEvents(GameEvent.PlayerTurnEnd)

                case Ranged.FireballScroll =>
                  RangedHelper
                    .useFireballScroll(
                      player,
                      gameMap.hostiles.findAllInRange(
                        target.position,
                        Ranged.FireballScroll.radius
                      )
                    )
                    .flatMap { consumed =>
                      if consumed then
                        player.removeInventoryItem(inventoryPosition).map {
                          nextPlayer =>
                            this.copy(player = nextPlayer).closeAllWindows
                        }
                      else Outcome(this)
                    }
                    .addGlobalEvents(GameEvent.PlayerTurnEnd)

    case GameEvent.HostileMeleeAttack(name, power) =>
      val damage = Math.max(
        0,
        power - (player.fighter.defense + player.equipment.defenseBonus)
      )

      val attackMessage =
        GameEvent.Log(
          if damage > 0 then
            Message(
              s"${name.capitalize} attacks for $damage hit points.",
              ColorScheme.enemyAttack
            )
          else
            Message(
              s"${name.capitalize} attacks but does no damage",
              ColorScheme.enemyAttack
            )
        )

      val p = player.takeDamage(damage)

      val msgs =
        if p.isAlive then Batch(attackMessage)
        else
          GameEvent.Log(
            Message("You died!", ColorScheme.playerDie)
          ) :: attackMessage :: Batch.empty

      Outcome(
        this.copy(
          player = p
        )
      ).addGlobalEvents(msgs)

    case GameEvent.HostileGiveXP(amount) =>
      player
        .addXp(amount)
        .map { p =>
          this.copy(
            player = p,
            currentState =
              if p.level > player.level then GameState.LevelUp
              else GameState.Game
          )
        }

    case GameEvent.PlayerAttack(name, power, id) =>
      gameMap.hostiles.findById(id) match
        case None =>
          Outcome(this.closeAllWindows)
            .addGlobalEvents(
              GameEvent.Log(
                Message(
                  s"${name.capitalize} swings and misses!",
                  ColorScheme.playerAttack
                )
              )
            )

        case Some(target) =>
          val damage = Math.max(0, power - target.fighter.defense)

          val msg =
            if damage > 0 then
              Message(
                s"${name.capitalize} attacks for $damage hit points.",
                ColorScheme.playerAttack
              )
            else
              Message(
                s"${name.capitalize} attacks but does no damage",
                ColorScheme.playerAttack
              )

          val res = gameMap
            .damageHostile(target.id, damage)

          val events = GameEvent.Log(msg) :: res.globalEventsOrNil.reverse

          res.clearGlobalEvents
            .map(gm =>
              this
                .copy(
                  gameMap = gm
                )
                .closeAllWindows
            )
            .addGlobalEvents(events)

    case GameEvent.PlayerCastsConfusion(name, numberOfTurns, id) =>
      gameMap.hostiles.findById(id) match
        case None =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(
                Message(s"${name.capitalize} misses!", ColorScheme.playerAttack)
              ),
              GameEvent.PlayerTurnEnd
            )

        case Some(target) =>
          gameMap
            .confuseHostile(target.id, numberOfTurns)
            .map(gm =>
              this.copy(
                gameMap = gm
              )
            )
            .addGlobalEvents(GameEvent.PlayerTurnEnd)

    case GameEvent.PlayerCastsFireball(name, damage, id) =>
      gameMap.hostiles.findById(id) match
        case None =>
          Outcome(this)
            .addGlobalEvents(
              GameEvent.Log(
                Message(s"${name.capitalize} misses!", ColorScheme.playerAttack)
              ),
              GameEvent.PlayerTurnEnd
            )

        case Some(target) =>
          gameMap
            .damageHostile(target.id, damage)
            .map(gm =>
              this.copy(
                gameMap = gm
              )
            )
            .addGlobalEvents(GameEvent.PlayerTurnEnd)

    case GameEvent.PlayerTurnEnd =>
      Outcome(this.copy(gamePhase = GamePhase.MovingPlayer))
        .addGlobalEvents(GameEvent.ViewModelHandOff(ViewModelEvent.MovePlayer))

    case GameEvent.PlayerMoveTowards(target) =>
      if autoMovePath.isEmpty then
        val p =
          gameMap.getPathTo(dice, player.position, target, Batch.empty).tail
        if p.nonEmpty then
          val next      = p.head
          val remaining = p.tail
          performPlayerTurn(dice, next - player.position).map {
            _.copy(
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
        performPlayerTurn(dice, autoMovePath.head - player.position).map {
          _.copy(
            autoMovePath = autoMovePath.tail
          )
        }

    case GameEvent.NPCTurnComplete =>
      if gameMap.noHostilesVisible then
        val events =
          if autoMovePath.isEmpty then Batch(GameEvent.Redraw)
          else Batch(GameEvent.Redraw, GameEvent.PlayerContinueMove)

        Outcome(this.copy(gamePhase = GamePhase.WaitForInput))
          .addGlobalEvents(events)
      else
        Outcome(
          this.copy(
            gamePhase = GamePhase.WaitForInput,
            autoMovePath = Batch.empty
          )
        ).addGlobalEvents(GameEvent.Redraw)

    case GameEvent.RedrawHistoryLog =>
      Outcome(this)

    case GameEvent.Redraw =>
      Outcome(this)

    case GameEvent.CameraSnapToPlayer =>
      Outcome(this)

    case GameEvent.ViewModelPhaseComplete(_) =>
      Outcome(this)

    case GameEvent.ViewModelHandOff(_) =>
      Outcome(this)

    case GameEvent.ModelHandOff(ViewModelEvent.MovePlayer) =>
      Outcome(this)

    case GameEvent.ModelHandOff(ViewModelEvent.PlayerMoveComplete) =>
      // This is a fudge. The player move is done and we should now check for
      // other things the player is doing, like attacking, then hand off to the
      // view model again for the next round of presentation.
      // What we actually do is the entire NPC moves, and immediately complete.
      gameMap
        .update(dice, player.position, paused)
        .map { gm =>
          this.copy(gameMap = gm, gamePhase = GamePhase.MovingNPC)
        }
        .addGlobalEvents(GameEvent.NPCTurnComplete)

  def performPlayerTurn(dice: Dice, by: Point): Outcome[Model] =
    player.bump(by, gameMap).map(p => this.copy(player = p))

  def moveUp(dice: Dice): Outcome[Model] = performPlayerTurn(dice, Point(0, -1))
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
    player.pickUp(gameMap.collectables).map { case (p, updatedCollectables) =>
      this.copy(
        player = p,
        gameMap = gameMap.copy(collectables = updatedCollectables)
      )
    }

  def toSaveData: ModelSaveData =
    ModelSaveData(
      screenSize,
      player,
      stairsPosition,
      gameMap,
      messageLog,
      currentFloor
    )

object Model:

  val HistoryWindowSize: Size   = Size(50, 36)
  val InventoryWindowSize: Size = Size(30, 10)
  val DropWindowSize: Size      = Size(30, 10)
  val QuitWindowSize: Size      = Size(30, 10)

  def blank(dice: Dice, screenSize: Size): Model =
    val p = Player.initial(dice, Point.zero)
    Model(
      screenSize,
      p,
      Point.zero,
      Point.zero,
      GameMap.initial(screenSize, Batch.empty, Batch.empty),
      MessageLog.DefaultLimited,
      false,
      GameState.Game,
      None,
      GameLoadInfo.initial,
      0,
      GamePhase.WaitForInput,
      Batch.empty
    )

  def fromSaveData(saveData: ModelSaveData): Model =
    // Can use Dice.fromSeed(0) here since player is overwritten with saved data.
    blank(Dice.fromSeed(0), saveData.screenSize).copy(
      player = saveData.player,
      stairsPosition = saveData.stairsPosition,
      gameMap = saveData.gameMap,
      messageLog = saveData.messageLog,
      loadInfo = GameLoadInfo.withSaveData(saveData),
      currentFloor = saveData.currentFloor
    )

  def gen(dice: Dice, screenSize: Size): Outcome[Model] =
    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        screenSize - Size(0, 5),
        DungeonGen.maxMonstersPerRoom(0),
        DungeonGen.maxCollectablesPerRoom(0),
        0
      )

    val p = Player.initial(dice, dungeon.playerStart)

    GameMap
      .gen(screenSize, dungeon)
      .update(dice, dungeon.playerStart, true)
      .map { gm =>
        Model(
          screenSize,
          p,
          dungeon.stairsPosition,
          Point.zero,
          gm,
          MessageLog.DefaultLimited,
          false,
          GameState.Game,
          None,
          GameLoadInfo.initial,
          0,
          GamePhase.WaitForInput,
          Batch.empty
        )
      }

  def genNextFloor(dice: Dice, currentModel: Model): Outcome[Model] =
    val nextFloor = currentModel.currentFloor + 1

    val dungeon =
      DungeonGen.makeMap(
        dice,
        DungeonGen.MaxRooms,
        DungeonGen.RoomMinSize,
        DungeonGen.RoomMaxSize,
        currentModel.screenSize - Size(0, 5),
        DungeonGen.maxMonstersPerRoom(nextFloor),
        DungeonGen.maxCollectablesPerRoom(nextFloor),
        nextFloor
      )

    GameMap
      .gen(currentModel.screenSize, dungeon)
      .update(dice, dungeon.playerStart, true)
      .map { gm =>
        currentModel.copy(
          player = currentModel.player.copy(position = dungeon.playerStart),
          stairsPosition = dungeon.stairsPosition,
          gameMap = gm,
          currentFloor = nextFloor
        )
      }
