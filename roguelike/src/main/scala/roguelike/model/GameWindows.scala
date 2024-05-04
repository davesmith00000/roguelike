package roguelike.model

import indigo._
import roguelike.assets.GameAssets
import roguelike.model.entity.Player
import roguelikestarterkit._

object GameWindows:

  val defaultCharSheet: CharSheet =
    CharSheet(
      GameAssets.assets.init.AnikkiSquare10x10,
      Size(10),
      RoguelikeTiles.Size10x10.charCrops,
      RoguelikeTiles.Size10x10.Fonts.fontKey
    )

final case class GameWindowContext(currentFloor: Int, player: Player, messageLog: MessageLog)
