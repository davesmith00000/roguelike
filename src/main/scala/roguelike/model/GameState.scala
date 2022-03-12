package roguelike.model

import indigo._

enum GameState:
  case Game                    extends GameState
  case History                 extends GameState
  case Inventory               extends GameState
  case Equipment               extends GameState
  case Drop                    extends GameState
  case LookAround(radius: Int) extends GameState
  case Quit                    extends GameState
  case LevelUp                 extends GameState

  def showingHistory: Boolean =
    this match
      case GameState.History => true
      case _                 => false

  def showingInventory: Boolean =
    this match
      case GameState.Inventory => true
      case _                   => false

  def showingEquipment: Boolean =
    this match
      case GameState.Equipment => true
      case _                   => false

  def showingDropMenu: Boolean =
    this match
      case GameState.Drop => true
      case _              => false

  def isRunning: Boolean =
    this match
      case GameState.Game => true
      case _              => false

  def lookingAround: Boolean =
    this match
      case GameState.LookAround(_) => true
      case _                       => false

  def showingQuit: Boolean =
    this match
      case GameState.Quit => true
      case _              => false

  def showingLevelUp: Boolean =
    this match
      case GameState.LevelUp => true
      case _                 => false
