package roguelike.model

import indigo.*
import indigo.json.Json
import roguelike.assets.AsepriteDeath
import roguelike.assets.AsepriteEnemy1
import roguelike.assets.AsepriteEnemy2
import roguelike.assets.AsepriteEnemy3
import roguelike.assets.AsepritePlayer
import roguelike.assets.GameAssets

object GameAssetLoader:

  def loadAnimation(
      assetCollection: AssetCollection,
      dice: Dice
  )(
      spriteData: SpriteAssetData,
      depth: Depth
  ): Outcome[SpriteAndAnimations] = {

    val res =
      spriteData.aseprite
        .toSpriteAndAnimations(dice, spriteData.imageData)
        .map(_.modifySprite(_.withDepth(depth)))

    res match
      case Some(spriteAndAnimations) =>
        Outcome(spriteAndAnimations)

      case None =>
        Outcome.raiseError(new Exception("Failed to load sprite from assets: " + spriteData))
  }

final case class SpriteAssetData(
    imageData: AssetName,
    aseprite: Aseprite
)
object SpriteAssetData:

  val spriteData = Batch(
    SpriteAssetData(
      GameAssets.assets.game.Player,
      AsepritePlayer.aseprite
    ),
    SpriteAssetData(
      GameAssets.assets.game.death,
      AsepriteDeath.aseprite
    ),
    SpriteAssetData(
      GameAssets.assets.game.enemy1,
      AsepriteEnemy1.aseprite
    ),
    SpriteAssetData(
      GameAssets.assets.game.ENEMY2,
      AsepriteEnemy2.aseprite
    ),
    SpriteAssetData(
      GameAssets.assets.game.ENEMY3,
      AsepriteEnemy3.aseprite
    )
  )
