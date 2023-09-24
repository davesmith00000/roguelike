package roguelike.assets

import indigo.platform.assets.AssetCollection
import indigo.shared.assets.AssetType
import roguelike.assets.GeneratedAssets

object GameAssets:

  export GeneratedAssets.*

  def initialAssets: Set[AssetType] =
    GeneratedAssets.assets.init.assetSet

  def loaded(assetCollection: AssetCollection): Boolean =
    GeneratedAssets.assets.game.assetSet.forall {
      case t: AssetType.Text =>
        assetCollection.findTextDataByName(t.name).isDefined

      case i: AssetType.Image =>
        assetCollection.findImageDataByName(i.name).isDefined

      case a: AssetType.Audio =>
        assetCollection.findAudioDataByName(a.name).isDefined

      case f: AssetType.Font =>
        assetCollection.findFontDataByName(f.name).isDefined

      case t: AssetType.Tagged =>
        t.images.forall(i => assetCollection.findImageDataByName(i.name).isDefined)
    }
