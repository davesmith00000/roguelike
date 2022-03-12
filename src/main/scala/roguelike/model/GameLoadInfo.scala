package roguelike.model

import indigo._

final case class GameLoadInfo(
    loadingTimeOut: Option[Seconds],
    loadedData: Option[ModelSaveData]
):
  def updateTimeout(delta: Seconds): GameLoadInfo =
    this.copy(
      loadingTimeOut = loadingTimeOut.map(t =>
        if (t - delta).toDouble <= 0.0 then Seconds.zero else t - delta
      )
    )
