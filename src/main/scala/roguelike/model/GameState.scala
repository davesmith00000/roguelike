package roguelike.model

import indigo._

enum GameState:
  case Game
  case LookAround(radius: Int)
  case ShowingWindow

  def isRunning: Boolean =
    this match
      case GameState.Game => true
      case _              => false

  def lookingAround: Boolean =
    this match
      case GameState.LookAround(_) => true
      case _                       => false
