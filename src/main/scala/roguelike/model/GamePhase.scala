package roguelike.model

enum GamePhase:
  case WaitForInput
  case MovingPlayer
  case UpdateNPC
  case MovingNPC

  def isUpdateNPC: Boolean =
    this == UpdateNPC
