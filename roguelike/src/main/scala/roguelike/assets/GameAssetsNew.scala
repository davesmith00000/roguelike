package roguelike.assets

import indigo.platform.assets.AssetCollection
import indigo.shared.assets.AssetType
import roguelike.assets.GeneratedAssets

object GameAssetsNew:

  def initialAssets: Set[AssetType] =
    Set()
  
  def loaded(assetCollection: AssetCollection): Boolean =
    audiosLoaded(assetCollection) &&
    imagesLoaded(assetCollection) &&
    textsLoaded(assetCollection) &&
    lazyAssetsLoaded(assetCollection)

  
  private def audiosLoaded(assetCollection: AssetCollection): Boolean =
    false
    // audioAssets.forall(a => assetCollection.findAudioDataByName(a).isDefined)
  
  private def imagesLoaded(assetCollection: AssetCollection): Boolean =
    false
    // imagesAssets.forall(i => assetCollection.findImageDataByName(i).isDefined)
  
  private def textsLoaded(assetCollection: AssetCollection): Boolean =
    false
    // textAssets.forall(t => assetCollection.findTextDataByName(t).isDefined)

  private def lazyAssetsLoaded(assetCollection: AssetCollection): Boolean =
    false
    // lazyAssets.forall {
    //   case t: AssetType.Text =>
    //     assetCollection.findTextDataByName(t.name).isDefined
  
    //   case i: AssetType.Image =>
    //     assetCollection.findImageDataByName(i.name).isDefined
  
    //   case a: AssetType.Audio =>
    //     assetCollection.findAudioDataByName(a.name).isDefined
  
    //   case f: AssetType.Font =>
    //     assetCollection.findFontDataByName(f.name).isDefined
  
    //   case t: AssetType.Tagged =>
    //     t.images.forall(i => assetCollection.findImageDataByName(i.name).isDefined)
    // }
