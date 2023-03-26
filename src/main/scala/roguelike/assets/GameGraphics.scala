package roguelike.assets

import indigo.*

object GameGraphics:

  val tileClone: CloneBlank =
    CloneBlank(
      CloneId("rogue bg tiles"),
      Graphic(32, 32, Material.Bitmap(GameAssets.Tileset))
    )

  val tileShadowClone: CloneBlank =
    CloneBlank(
      CloneId("rogue bg shadow tiles"),
      Graphic(
        32,
        32,
        Material
          .Bitmap(GameAssets.Tileset)
          .toImageEffects
          .withOverlay(Fill.Color(RGBA.Black.withAmount(0.7)))
      )
    )

  // Bg tiles
  def wallTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 0, 96, 32, 32)

  def floorTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 384, 0, 32, 32)

  def stairsTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 32, 288, 32, 32)

  //

  val highlight: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(0, 48, 24, 24)

  val target: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(24, 48, 24, 24)

  val fireScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(0, 72, 24, 24)
  val lightningScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(24, 72, 24, 24)
  val confusionScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(0, 96, 24, 24)
  val scrollInShadow: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(24, 96, 24, 24)

  val healthPotion: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(48, 96, 24, 24)

  val leatherArmour: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(72, 72, 24, 24)
  val chainmail: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(96, 72, 24, 24)
  val dagger: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(72, 96, 24, 24)
  val sword: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(GameAssets.Icons)).withCrop(96, 96, 24, 24)
