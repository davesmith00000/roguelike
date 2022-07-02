package roguelike.game

import indigo._
import indigo.scenes.SceneEvent
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.MainMenuScene
import roguelike.components.entities.HostilesManager
import roguelike.components.windows.*
import roguelike.model.GameLoadInfo
import roguelike.model.GameState
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.model.gamedata.KeyMapping

object GameSceneUpdate:

  def updateModel(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    model.gameState match
      case _ if WindowManager.showingWindow(model) =>
        onWaitingForInput(context, model)
      case GameState.WaitForInput   => onWaitingForInput(context, model)
      case GameState.UpdateNPCs     => updateNpcPhase(context, model)
      case GameState.UpdatingPlayer => otherPhase(context, model)
      case GameState.LookAround(_)  => otherPhase(context, model)
      case GameState.ShowingWindow  => otherPhase(context, model)

  def onWaitingForInput(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    // Window close keys
    case KeyboardEvent.KeyUp(KeyMapping.CloseWindow)
        if !model.gameState.isWaitForInput ||
          WindowManager.showingCloseableWindow(model) =>
      WindowManager
        .updateModel(context, model, WindowManagerCommand.CloseAll)
        .map(_.closeAllWindows)

    // Quit window toggle
    case KeyboardEvent.KeyUp(KeyMapping.Quit1) |
        KeyboardEvent.KeyUp(KeyMapping.Quit2)
        if model.gameState.isWaitForInput =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.HandleQuitKeyPress
      )

    // Delegate input to WindowManager
    case KeyboardEvent.KeyUp(key) if WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.DelegateInput(key)
      )

    // Looking around
    case KeyboardEvent.KeyDown(KeyMapping.MoveUp)
        if model.gameState.lookingAround =>
      model.lookUp

    case KeyboardEvent.KeyDown(KeyMapping.MoveDown)
        if model.gameState.lookingAround =>
      model.lookDown

    case KeyboardEvent.KeyDown(KeyMapping.MoveLeft)
        if model.gameState.lookingAround =>
      model.lookLeft

    case KeyboardEvent.KeyDown(KeyMapping.MoveRight)
        if model.gameState.lookingAround =>
      model.lookRight

    // Game controls
    case KeyboardEvent.KeyDown(KeyMapping.MoveUp)
        if model.gameState.isWaitForInput && model.player.isAlive =>
      model.moveUp(context.dice)

    case KeyboardEvent.KeyDown(KeyMapping.MoveDown)
        if model.gameState.isWaitForInput && model.player.isAlive =>
      model.moveDown(context.dice)

    case KeyboardEvent.KeyDown(KeyMapping.MoveLeft)
        if model.gameState.isWaitForInput && model.player.isAlive =>
      model.moveLeft(context.dice)

    case KeyboardEvent.KeyDown(KeyMapping.MoveRight)
        if model.gameState.isWaitForInput && model.player.isAlive =>
      model.moveRight(context.dice)

    case KeyboardEvent.KeyUp(KeyMapping.PickUp)
        if model.gameState.isWaitForInput && model.player.isAlive =>
      Outcome(model, Batch(GameEvent.PlayerTryPickUp))

    case KeyboardEvent.KeyUp(KeyMapping.Descend)
        if model.gameState.isWaitForInput && model.player.isAlive =>
      if model.player.position == model.stairsPosition then
        Outcome(model)
          .addGlobalEvents(
            GameEvent
              .Log(Message("You descend the staircase.", ColorScheme.descend)),
            GameEvent.PlayerDescended,
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
        if model.gameState.isWaitForInput ||
          !WindowManager.showingWindow(model) =>
      WindowManager
        .updateModel(context, model, WindowManagerCommand.ShowHistory)

    case KeyboardEvent.KeyUp(KeyMapping.Inventory)
        if model.gameState.isWaitForInput ||
          !WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.ShowInventoryMenu
      )

    case KeyboardEvent.KeyUp(KeyMapping.Equipment)
        if model.gameState.isWaitForInput ||
          !WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.ShowEquipMenu
      )

    case KeyboardEvent.KeyUp(KeyMapping.Drop)
        if model.gameState.isWaitForInput ||
          !WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.ShowDropMenu
      )

    // Look Around
    case KeyboardEvent.KeyUp(KeyMapping.LookAround)
        if model.gameState.isWaitForInput || model.gameState.lookingAround =>
      Outcome(model.toggleLookAround(0))

    case KeyboardEvent.KeyUp(KeyMapping.Target)
        if model.gameState.lookingAround =>
      Outcome(model.toggleLookAround(0))
        .addGlobalEvents(GameEvent.Targeted(model.lookAtTarget))

    // Other
    case e: GameEvent =>
      model.update(context)(e)

    case _ =>
      Outcome(model)

  def updateNpcPhase(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    case FrameTick if model.gameState.isUpdatingNPCs =>
      HostilesManager.updateModel(
        context,
        model,
        HostilesManager.Cmds.Update(model.gameMap)
      )

    // Other
    case e: GameEvent =>
      model.update(context)(e)

    case _ =>
      Outcome(model)

  def otherPhase(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    // Other
    case e: GameEvent =>
      model.update(context)(e)

    case _ =>
      Outcome(model)
