package roguelike.model

import indigo._

enum GameState:
  case Game
  case History
  case Inventory
  case Equipment
  case LookAround(radius: Int)
  case ShowingWindow

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

  def isRunning: Boolean =
    this match
      case GameState.Game => true
      case _              => false

  def lookingAround: Boolean =
    this match
      case GameState.LookAround(_) => true
      case _                       => false
