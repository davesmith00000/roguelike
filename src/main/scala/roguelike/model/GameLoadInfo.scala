package roguelike.model

import indigo.*

final case class GameLoadInfo(
    state: LoadingState,
    loadingTimeOut: Option[Seconds],
    loadedData: Option[ModelSaveData]
):
  def noTimeLeft: Boolean =
    loadingTimeOut match
      case Some(t) if t.toDouble <= 0.0 => true
      case _                            => false

  def isReallyComplete: Boolean =
    state.isComplete && (loadedData.isDefined || noTimeLeft)

  def start: GameLoadInfo =
    this.copy(state = LoadingState.InProgress(Some(0)))

  def updateTimeout(delta: Seconds): GameLoadInfo =
    this.copy(
      loadingTimeOut = loadingTimeOut.map(t =>
        if (t - delta).toDouble <= 0.0 then Seconds.zero else t - delta
      )
    )

object GameLoadInfo:
  def initial: GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, None, None)

  def withTimeOut(loadingTimeOut: Seconds): GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, Option(loadingTimeOut), None)

  def withSaveData(saveData: ModelSaveData): GameLoadInfo =
    GameLoadInfo(LoadingState.NotStarted, None, Option(saveData))

enum LoadingState:
  case NotStarted
  case InProgress(percent: Option[Int])
  case Complete
  case Error

  def isComplete: Boolean =
    this match
      case LoadingState.Complete => true
      case _                     => false
