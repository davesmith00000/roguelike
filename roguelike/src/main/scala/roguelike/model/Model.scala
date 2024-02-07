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
import roguelike.assets.GameAssets
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
import roguelikestarterkit.*

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
    activeWindow: ActiveWindow,
    collectables: Batch[Collectable],
    hostiles: HostilesPool,
    windowManager: WindowManagerModel[Size, Unit]
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

  val defaultCharSheet: CharSheet =
    CharSheet(
      GameAssets.assets.init.AnikkiSquare10x10,
      Size(10),
      RoguelikeTiles.Size10x10.charCrops
    )

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
      HostilesPool(Batch.empty),
      WindowManagerModel.initial
        .add(
          ColourWindow.window(
            defaultCharSheet
          )
        )
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
              HostilesPool(Batch.fromList(dungeon.hostiles)),
              WindowManagerModel.initial
                .add(
                  ColourWindow.window(
                    defaultCharSheet
                  )
                )
            )
        }
      }

// TODO: Test window! Remove!
object ColourWindow {

  final case class ColorPaletteReference(name: String, count: Int, colors: Batch[RGBA])

  val outrunner16 = ColorPaletteReference(
    "outrunner-16",
    16,
    Batch(
      RGBA.fromHexString("4d004c"),
      RGBA.fromHexString("8f0076"),
      RGBA.fromHexString("c70083"),
      RGBA.fromHexString("f50078"),
      RGBA.fromHexString("ff4764"),
      RGBA.fromHexString("ff9393"),
      RGBA.fromHexString("ffd5cc"),
      RGBA.fromHexString("fff3f0"),
      RGBA.fromHexString("000221"),
      RGBA.fromHexString("000769"),
      RGBA.fromHexString("00228f"),
      RGBA.fromHexString("0050c7"),
      RGBA.fromHexString("008bf5"),
      RGBA.fromHexString("00bbff"),
      RGBA.fromHexString("47edff"),
      RGBA.fromHexString("93fff8")
    )
  )

  final case class ColorPalette(components: ComponentGroup)

  private val graphic = Graphic(0, 0, TerminalMaterial(AssetName(""), RGBA.White, RGBA.Black))

  def window(
      charSheet: CharSheet
  ): WindowModel[Size, Unit, ColorPalette] =
    WindowModel(
      WindowId("Color palette"),
      charSheet,
      ColorPalette(
        ComponentGroup(Bounds(0, 0, 23, 23))
          .withLayout(ComponentLayout.Vertical())
          .add(
            ComponentGroup(Bounds(0, 0, 23, 10))
              .withLayout(ComponentLayout.Horizontal(Overflow.Wrap))
              .add(
                outrunner16.colors.map { rgba =>
                  Button(Bounds(0, 0, 3, 3))(presentSwatch(charSheet, rgba, None))
                    // .onClick(<Emit some event...>)
                    .presentOver(presentSwatch(charSheet, rgba, Option(RGBA.White)))
                    .presentDown(presentSwatch(charSheet, rgba, Option(RGBA.Black)))
                }
              )
          )
          .add(
            Button(Bounds(0, 0, 14, 3))(
              presentButton(charSheet, "Load palette", RGBA.Silver, RGBA.Black)
            )
              // .onClick(<Emit some event...>)
              .presentOver(presentButton(charSheet, "Load palette", RGBA.White, RGBA.Black))
              .presentDown(presentButton(charSheet, "Load palette", RGBA.Black, RGBA.White))
          )
      )
    )
      .withTitle("Colour Palette")
      .moveTo(0, 0)
      .resizeTo(25, 25)
      .isDraggable
      .isResizable
      .isCloseable
      .updateModel(updateModel)
      .present(present)

  def updateModel(
      context: UiContext[Size, Unit],
      model: ColorPalette
  ): GlobalEvent => Outcome[ColorPalette] =
    case e =>
      model.components.update(context)(e).map { c =>
        model.copy(components = c)
      }

  def present(
      context: UiContext[Size, Unit],
      model: ColorPalette
  ): Outcome[SceneUpdateFragment] =
    model.components.present(context).map { c =>
      SceneUpdateFragment(c.nodes).addCloneBlanks(c.cloneBlanks)
    }

  def presentSwatch(
      charSheet: CharSheet,
      colour: RGBA,
      stroke: Option[RGBA]
  ): (Coords, Bounds) => Outcome[ComponentFragment] =
    (offset, bounds) =>
      Outcome(
        ComponentFragment(
          stroke match
            case None =>
              Shape.Box(
                Rectangle(
                  offset.toScreenSpace(charSheet.size),
                  bounds.dimensions.toScreenSpace(charSheet.size)
                ),
                Fill.Color(colour)
              )

            case Some(strokeColor) =>
              Shape.Box(
                Rectangle(
                  offset.toScreenSpace(charSheet.size),
                  bounds.dimensions.toScreenSpace(charSheet.size)
                ),
                Fill.Color(colour),
                Stroke(2, strokeColor)
              )
        )
      )

  def presentButton(
      charSheet: CharSheet,
      text: String,
      fgColor: RGBA,
      bgColor: RGBA
  ): (Coords, Bounds) => Outcome[ComponentFragment] =
    (offset, bounds) =>
      val hBar = Batch.fill(text.length)("─").mkString
      val size = bounds.dimensions.unsafeToSize

      val terminal =
        RogueTerminalEmulator(size)
          .put(Point(0, 0), Tile.`┌`, fgColor, bgColor)
          .put(Point(size.width - 1, 0), Tile.`┐`, fgColor, bgColor)
          .put(Point(0, size.height - 1), Tile.`└`, fgColor, bgColor)
          .put(Point(size.width - 1, size.height - 1), Tile.`┘`, fgColor, bgColor)
          .put(Point(0, 1), Tile.`│`, fgColor, bgColor)
          .put(Point(size.width - 1, 1), Tile.`│`, fgColor, bgColor)
          .putLine(Point(1, 0), hBar, fgColor, bgColor)
          .putLine(Point(1, 1), text, fgColor, bgColor)
          .putLine(Point(1, 2), hBar, fgColor, bgColor)
          .toCloneTiles(
            CloneId("button"),
            bounds.coords
              .toScreenSpace(charSheet.size)
              .moveBy(offset.toScreenSpace(charSheet.size)),
            charSheet.charCrops
          ) { case (fg, bg) =>
            graphic.withMaterial(TerminalMaterial(charSheet.assetName, fg, bg))
          }

      Outcome(
        ComponentFragment(
          terminal.clones
        ).addCloneBlanks(terminal.blanks)
      )
}

final case class ColorPalette(components: ComponentGroup)
