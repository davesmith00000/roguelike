package roguelike

import indigo._
import indigo.json.Json

object AssetLoader:
  def loadAnimation(
      assetCollection: AssetCollection,
      dice: Dice
  )(
      jsonRef: AssetName,
      name: AssetName,
      depth: Depth
  ): Either[String, SpriteAndAnimations] = {
    given CanEqual[Option[SpriteAndAnimations], Option[SpriteAndAnimations]] =
      CanEqual.derived

    val res = for {
      json                <- assetCollection.findTextDataByName(jsonRef)
      aseprite            <- Json.asepriteFromJson(json)
      spriteAndAnimations <- aseprite.toSpriteAndAnimations(dice, name)
    } yield spriteAndAnimations.copy(sprite =
      spriteAndAnimations.sprite.withDepth(depth)
    )

    res match {
      case Some(spriteAndAnimations) =>
        Right(spriteAndAnimations)

      case None =>
        Left("Failed to load " + name)
    }
  }
