package roguelike

import indigo.*
import indigo.json.Json
import roguelike.assets.GameAssets

object AssetLoader:

  def loadAnimation(
      assetCollection: AssetCollection,
      dice: Dice
  )(
      spriteData: SpriteAssetData,
      depth: Depth
  ): Outcome[SpriteAndAnimations] = {

    val res = for {
      json                <- assetCollection.findTextDataByName(spriteData.jsonData)
      aseprite            <- Json.asepriteFromJson(json)
      spriteAndAnimations <- aseprite.toSpriteAndAnimations(dice, spriteData.imageData)
    } yield spriteAndAnimations.copy(sprite = spriteAndAnimations.sprite.withDepth(depth))

    res match
      case Some(spriteAndAnimations) =>
        Outcome(spriteAndAnimations)

      case None =>
        Outcome.raiseError(new Exception("Failed to load sprite from assets: " + spriteData))
  }

final case class SpriteAssetData(
    imageData: AssetName,
    jsonData: AssetName
)
object SpriteAssetData:

  val spriteData = Batch(
    SpriteAssetData(GameAssets.Player, GameAssets.PlayerData),
    SpriteAssetData(GameAssets.Death, GameAssets.DeathData),
    SpriteAssetData(GameAssets.Enemy1, GameAssets.EnemyData1),
    SpriteAssetData(GameAssets.Enemy2, GameAssets.EnemyData2),
    SpriteAssetData(GameAssets.Enemy3, GameAssets.EnemyData3)
  )
