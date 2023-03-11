package roguelike.model

import indigo.*

final case class GameLoadInfo(
    state: LoadingState,
    loadedData: Option[ModelSaveData]
):
  def start: GameLoadInfo =
    this.copy(state = LoadingState.InProgress(Some(0)))

  def toInProgress(percent: Int): GameLoadInfo =
    this.copy(state = LoadingState.InProgress(Option(percent)))

  def toComplete: GameLoadInfo =
    this.copy(state = LoadingState.Complete)

  def toError(msg: String): GameLoadInfo =
    this.copy(state = LoadingState.Error(msg))

  def withSaveData(data: ModelSaveData): GameLoadInfo =
    this.copy(loadedData = Option(data))

object GameLoadInfo:
  def initial: GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, None)

  def withSaveData(saveData: ModelSaveData): GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, Option(saveData))

enum LoadingState:
  case NotStarted
  case InProgress(percent: Option[Int])
  case Complete
  case Error(msg: String)

  def isComplete: Boolean =
    this match
      case LoadingState.Complete => true
      case _                     => false
