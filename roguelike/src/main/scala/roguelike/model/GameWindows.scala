package roguelike.model

import indigo.*
import roguelike.assets.GameAssets
import roguelike.model.entity.Player
import roguelike.windows.HistoryWindow
import roguelike.windows.InfoPanel
import roguelike.windows.MenuWindow
import roguelikestarterkit.*

object GameWindows:

  val defaultCharSheet: CharSheet =
    CharSheet(
      GameAssets.assets.init.AnikkiSquare10x10,
      Size(10),
      RoguelikeTiles.Size10x10.charCrops,
      RoguelikeTiles.Size10x10.Fonts.fontKey
    )

final case class GameWindowContext(currentFloor: Int, player: Player, messageLog: MessageLog)
