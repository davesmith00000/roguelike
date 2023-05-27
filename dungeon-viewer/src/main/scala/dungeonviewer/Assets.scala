package dungeonviewer

import indigo.*

object Assets:

  val tileMap = AssetName("Anikki_square_10x10")

  val assets: Set[AssetType] =
    Set(
      AssetType.Image(tileMap, AssetPath("assets/" + tileMap.toString + ".png"))
    )

end Assets
