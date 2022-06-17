package roguelike.game

import indigo._
import indigo.scenes.SceneEvent
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.MainMenuScene
import roguelike.components.windows.*
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
      case GamePhase.MovingPlayer => otherPhase(context, model)
      case GamePhase.MovingNPC    => otherPhase(context, model)

  def onWaitingForInput(
      context: FrameContext[Size],
      model: Model
  ): GlobalEvent => Outcome[Model] =
    // Window close keys
    case KeyboardEvent.KeyUp(KeyMapping.CloseWindow)
        if !model.currentState.isRunning ||
          WindowManager.showingCloseableWindow(model) =>
      WindowManager
        .updateModel(context, model, WindowManagerCommand.CloseAll)
        .map(_.closeAllWindows)

    // Quit window toggle
    case KeyboardEvent.KeyUp(KeyMapping.Quit1) |
        KeyboardEvent.KeyUp(KeyMapping.Quit2) if model.currentState.isRunning =>
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
        if model.currentState.isRunning ||
          !WindowManager.showingWindow(model) =>
      WindowManager
        .updateModel(context, model, WindowManagerCommand.ShowHistory)
        .addGlobalEvents(GameEvent.RedrawHistoryLog)

    case KeyboardEvent.KeyUp(KeyMapping.Inventory)
        if model.currentState.isRunning ||
          !WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.ShowInventoryMenu
      )

    case KeyboardEvent.KeyUp(KeyMapping.Equipment)
        if model.currentState.isRunning ||
          !WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.ShowEquipMenu
      )

    case KeyboardEvent.KeyUp(KeyMapping.Drop)
        if model.currentState.isRunning ||
          !WindowManager.showingWindow(model) =>
      WindowManager.updateModel(
        context,
        model,
        WindowManagerCommand.ShowDropMenu
      )

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
