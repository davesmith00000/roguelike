package roguelike.model

import indigo._

enum GameState:
  case Game
  case History
  case Inventory
  case Equipment
  case Drop
  case LookAround(radius: Int)
  case LevelUp

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

  def showingLevelUp: Boolean =
    this match
      case GameState.LevelUp => true
      case _                 => false
