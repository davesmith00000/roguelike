package roguelike.assets

import indigo.*

object Assets:

  object Basic:
    val textFragShader     = AssetName("text frag")
    val tileMap: AssetName = AssetName("Anikki_square_10x10")

    val assets: Set[AssetType] =
      Set(
        AssetType.Text(textFragShader, AssetPath("assets/shaders/text.frag")),
        AssetType
          .Image(tileMap, AssetPath("assets/" + tileMap.toString + ".png"))
      )
  end Basic

  object Game:
    val icons: AssetName = AssetName("simple-sprites-24x24")

    val assets: Set[AssetType] =
      Set(
        AssetType.Image(icons, AssetPath("assets/" + icons.toString + ".png"))
      )

    def loaded(assetCollection: AssetCollection): Boolean =
      assetCollection.findImageDataByName(icons).isDefined

  end Game

end Assets


