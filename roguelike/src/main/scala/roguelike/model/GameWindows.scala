package roguelike.model

import indigo.*
import roguelike.assets.GameAssets
import roguelike.components.windows.InfoPanel
import roguelike.components.windows.MenuWindow
import roguelike.model.entity.Player
import roguelikestarterkit.*

object GameWindows:

  val defaultCharSheet: CharSheet =
    CharSheet(
      GameAssets.assets.init.AnikkiSquare10x10,
      Size(10),
      RoguelikeTiles.Size10x10.charCrops
    )

  def addWindows(
      windowManager: WindowManagerModel[Size, GameWindowContext]
  ): WindowManagerModel[Size, GameWindowContext] =
    windowManager
      .add(
        InfoPanel.window(defaultCharSheet)
      )
      .add(
        MenuWindow.window(
          Dimensions(1280, 720) / 10,
          defaultCharSheet
        )
      )

final case class GameWindowContext(currentFloor: Int, player: Player)
