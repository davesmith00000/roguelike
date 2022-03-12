package roguelike.model

enum GamePhase:
  case WaitForInput, PlayerTurn, NPCTurn

  def isWaitingForInput: Boolean =
    this match
      case WaitForInput => true
      case PlayerTurn   => false
      case NPCTurn      => false

  def isPlayerTurn: Boolean =
    this match
      case WaitForInput => false
      case PlayerTurn   => true
      case NPCTurn      => false

  def isNPCTurn: Boolean =
    this match
      case WaitForInput => false
      case NPCTurn      => true
      case PlayerTurn   => false
