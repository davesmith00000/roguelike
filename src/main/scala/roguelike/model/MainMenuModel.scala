package roguelike.model

import indigo.shared.time.Seconds

final case class MainMenuModel(
    gameData: Model,
    sceneTime: Seconds,
    showMenu: Boolean
)
