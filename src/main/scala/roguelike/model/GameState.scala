package roguelike.model

import indigo._

enum GameState:
  case WaitForInput
  case UpdatingPlayer
  case UpdateNPCs
  case LookAround(radius: Int)
  case ShowingWindow

  def isWaitForInput: Boolean =
    this == GameState.WaitForInput

  def lookingAround: Boolean =
    this match
      case GameState.LookAround(_) => true
      case _                       => false

  def isUpdatingNPCs: Boolean =
    this == UpdateNPCs
