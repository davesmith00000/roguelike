package roguelike.model

import indigo.*
import roguelike.assets.GameAssets
import roguelike.model.entity.Collectable
import roguelike.model.entity.Player
import roguelikestarterkit.*

object GameWindows:

  val defaultCharSheet: CharSheet =
    CharSheet(
      GameAssets.assets.init.AnikkiSquare10x10,
      Size(10),
      RoguelikeTiles.Size10x10.charCrops,
      RoguelikeTiles.Size10x10.Fonts.fontKey
    )

final case class GameWindowContext(currentFloor: Int, player: Player, standingOn: Option[Collectable], messageLog: MessageLog)
