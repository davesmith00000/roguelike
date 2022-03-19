package roguelike.game

import indigo._
import indigo.scenes.SceneEvent
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.MainMenuScene
import roguelike.model.GameLoadInfo
import roguelike.model.GamePhase
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.model.gamedata.KeyMapping

object GameSceneUpdate:

  def updateModel(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    model.gamePhase match
      case GamePhase.WaitForInput => onWaitingForInput(context, model)
      case GamePhase.PlayerTurn   => otherPhase(context, model)
      case GamePhase.NPCTurn      => otherPhase(context, model)

  def onWaitingForInput(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    // Window close keys
    case KeyboardEvent.KeyUp(KeyMapping.CloseWindow)
        if !model.currentState.isRunning || !model.currentState.showingLevelUp =>
      Outcome(model.closeAllWindows)

    // Quit window
    // Save
    case KeyboardEvent.KeyUp(Key.KEY_1)
        if model.currentState.showingQuit && model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(model.copy(loadInfo = GameLoadInfo.withSaveData(saveData)))
        .addGlobalEvents(
          StorageEvent.Save(
            ModelSaveData.saveKey,
            model.toSaveData.toJsonString
          )
        )

    // Save and Quit
    case KeyboardEvent.KeyUp(Key.KEY_2)
        if model.currentState.showingQuit && model.player.isAlive =>
      val saveData = model.toSaveData
      Outcome(model.copy(loadInfo = GameLoadInfo.withSaveData(saveData)))
        .addGlobalEvents(
          StorageEvent.Save(ModelSaveData.saveKey, saveData.toJsonString),
          SceneEvent.JumpTo(MainMenuScene.name)
        )

    // Quit
    case KeyboardEvent.KeyUp(Key.KEY_3) if model.currentState.showingQuit =>
      Outcome(model).addGlobalEvents(SceneEvent.JumpTo(MainMenuScene.name))

    // Level up window
    // Constitution
    case KeyboardEvent.KeyUp(Key.KEY_1) if model.currentState.showingLevelUp =>
      model.player
        .increaseMaxHp(20)
        .map { p =>
          model.copy(player = p).toggleLevelUp
        }

    // Strength
    case KeyboardEvent.KeyUp(Key.KEY_2) if model.currentState.showingLevelUp =>
      model.player
        .increasePower(1)
        .map { p =>
          model.copy(player = p).toggleLevelUp
        }

    // Agility
    case KeyboardEvent.KeyUp(Key.KEY_3) if model.currentState.showingLevelUp =>
      model.player
        .increaseDefense(1)
        .map { p =>
          model.copy(player = p).toggleLevelUp
        }

    // Invalid level up selection
    case KeyboardEvent.KeyUp(_) if model.currentState.showingLevelUp =>
      Outcome(model).addGlobalEvents(
        GameEvent.Log(
          Message("Invalid, please press 1, 2, or 3.", ColorScheme.invalid)
        )
      )

    // Inventory window
    case KeyboardEvent.KeyUp(key) if model.currentState.showingInventory =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(keyIndex) =>
          model.player
            .useInventoryItem(keyIndex)
            .map { p =>
              model.copy(
                player = p
              )
            }

    // Equipment window
    case KeyboardEvent.KeyUp(key) if model.currentState.showingEquipment =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(0) =>
          model.player.unequipArmour
            .map(p => model.copy(player = p))

        case Some(1) =>
          model.player.unequipMelee
            .map(p => model.copy(player = p))

        case _ =>
          Outcome(model)

    // Drop window
    case KeyboardEvent.KeyUp(key) if model.currentState.showingDropMenu =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(keyIndex) =>
          model.player
            .drop(keyIndex, model.gameMap.collectables)
            .map(p => model.copy(player = p))

    // Looking around
    case KeyboardEvent.KeyDown(KeyMapping.MoveUp)
        if model.currentState.lookingAround =>
      model.lookUp

    case KeyboardEvent.KeyDown(KeyMapping.MoveDown)
        if model.currentState.lookingAround =>
      model.lookDown

    case KeyboardEvent.KeyDown(KeyMapping.MoveLeft)
        if model.currentState.lookingAround =>
      model.lookLeft

    case KeyboardEvent.KeyDown(KeyMapping.MoveRight)
        if model.currentState.lookingAround =>
      model.lookRight

    // Game controls
    case KeyboardEvent.KeyDown(KeyMapping.MoveUp)
        if model.currentState.isRunning && model.player.isAlive =>
      model.moveUp(context.dice)

    case KeyboardEvent.KeyDown(KeyMapping.MoveDown)
        if model.currentState.isRunning && model.player.isAlive =>
      model.moveDown(context.dice)

    case KeyboardEvent.KeyDown(KeyMapping.MoveLeft)
        if model.currentState.isRunning && model.player.isAlive =>
      model.moveLeft(context.dice)

    case KeyboardEvent.KeyDown(KeyMapping.MoveRight)
        if model.currentState.isRunning && model.player.isAlive =>
      model.moveRight(context.dice)

    case KeyboardEvent.KeyUp(KeyMapping.PickUp)
        if model.currentState.isRunning && model.player.isAlive =>
      model.pickUp

    case KeyboardEvent.KeyUp(KeyMapping.Descend)
        if model.currentState.isRunning && model.player.isAlive =>
      if model.player.position == model.stairsPosition then
        Model
          .genNextFloor(context.dice, model)
          .addGlobalEvents(
            GameEvent
              .Log(Message("You descend the staircase.", ColorScheme.descend)),
            GameEvent.CameraSnapToPlayer,
            GameEvent.PlayerTurnEnd
          )
      else
        Outcome(model).addGlobalEvents(
          GameEvent.Log(
            Message("There are no stairs here.", ColorScheme.impossible)
          )
        )

    // Window toggles
    case KeyboardEvent.KeyUp(KeyMapping.ViewHistory)
        if model.currentState.isRunning || model.currentState.showingHistory =>
      Outcome(model.toggleMessageHistory)
        .addGlobalEvents(GameEvent.RedrawHistoryLog)

    case KeyboardEvent.KeyUp(KeyMapping.Inventory)
        if model.currentState.isRunning || model.currentState.showingInventory =>
      Outcome(model.toggleInventory)

    case KeyboardEvent.KeyUp(KeyMapping.Equipment)
        if model.currentState.isRunning || model.currentState.showingEquipment =>
      Outcome(model.toggleEquipment)

    case KeyboardEvent.KeyUp(KeyMapping.Drop)
        if model.currentState.isRunning || model.currentState.showingDropMenu =>
      Outcome(model.toggleDropMenu)

    case KeyboardEvent.KeyUp(KeyMapping.Quit1) |
        KeyboardEvent.KeyUp(KeyMapping.Quit2)
        if model.currentState.isRunning || model.currentState.showingQuit =>
      Outcome(model.toggleQuit)

    // Look Around
    case KeyboardEvent.KeyUp(KeyMapping.LookAround)
        if model.currentState.isRunning || model.currentState.lookingAround =>
      Outcome(model.toggleLookAround(0))

    case KeyboardEvent.KeyUp(KeyMapping.Target)
        if model.currentState.lookingAround =>
      Outcome(model.toggleLookAround(0))
        .addGlobalEvents(GameEvent.Targeted(model.lookAtTarget))

    // Other
    case e: GameEvent =>
      model.update(context.dice)(e)

    case _ =>
      Outcome(model)

  def otherPhase(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    // Other
    case e: GameEvent =>
      model.update(context.dice)(e)

    case _ =>
      Outcome(model)
