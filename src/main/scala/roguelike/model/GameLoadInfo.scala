package roguelike.model

import indigo.*

final case class GameLoadInfo(
    state: LoadingState,
    loadedData: Option[ModelSaveData]
):
  def start: GameLoadInfo =
    this.copy(state = LoadingState.InProgress(Some(0)))

object GameLoadInfo:
  def initial: GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, None)

  def withTimeOut(loadingTimeOut: Seconds): GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, None)

  def withSaveData(saveData: ModelSaveData): GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, Option(saveData))

enum LoadingState:
  case NotStarted
  case InProgress(percent: Option[Int])
  case Complete
  case Error

  def isComplete: Boolean =
    this match
      case LoadingState.Complete => true
      case _                     => false
