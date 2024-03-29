package roguelike.assets

import indigo.*
import roguelike.model.dungeon.WallCode

object GameGraphics:

  val tileClone: CloneBlank =
    CloneBlank(
      CloneId("rogue bg tiles"),
      Graphic(32, 32, GameAssets.assets.game.tilesetMaterial)
    )

  val tileShadowClone: CloneBlank =
    CloneBlank(
      CloneId("rogue bg shadow tiles"),
      Graphic(
        32,
        32,
        GameAssets.assets.game.tilesetMaterial.toImageEffects
          .withOverlay(Fill.Color(RGBA.Black.withAmount(0.7)))
      )
    )

  // Bg tiles
  def wallTile(code: WallCode, position: Point): CloneTileData =
    code match
      case WallCode.DropOff =>
        CloneTileData(position.x, position.y, 160, 256, 32, 32)

      case WallCode.DropOffFadeLeft =>
        CloneTileData(position.x, position.y, 160 + 32, 256, 32, 32)

      case WallCode.DropOffMiddle =>
        CloneTileData(position.x, position.y, 160 + 32 + 32, 256, 32, 32)

      case WallCode.DropOffFadeRight =>
        CloneTileData(position.x, position.y, 160 + 32 + 32 + 32, 256, 32, 32)

      case WallCode.Wall =>
        CloneTileData(position.x, position.y - 32, 96, 224, 32, 64)

  def floorTile(style: Int, position: Point): CloneTileData =
    val startX = 384
    val startY = 0
    style match
      case 1 => CloneTileData(position.x, position.y, startX + 32, startY, 32, 32)
      case 2 => CloneTileData(position.x, position.y, startX + 32 + 32, startY, 32, 32)
      case 3 => CloneTileData(position.x, position.y, startX + 32 + 32 + 32, startY, 32, 32)
      case 4 => CloneTileData(position.x, position.y, startX + 32, startY + 32, 32, 32)
      case 5 => CloneTileData(position.x, position.y, startX + 32 + 32, startY + 32, 32, 32)
      case 6 => CloneTileData(position.x, position.y, startX + 32 + 32 + 32, startY + 32, 32, 32)
      case 7 => CloneTileData(position.x, position.y, startX + 32 + 32, startY + 32 + 32, 32, 32)
      case _ => // includes 0
        CloneTileData(position.x, position.y, startX, startY, 32, 32)

  def stairsTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 32, 288, 32, 32)

  //

  val target: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(24, 48, 24, 24)

  val fireScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(0, 72, 24, 24)
  val lightningScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(24, 72, 24, 24)
  val confusionScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(0, 96, 24, 24)
  val scrollInShadow: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(24, 96, 24, 24)

  val healthPotion: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(48, 96, 24, 24)

  val leatherArmour: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(72, 72, 24, 24)
  val chainmail: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(96, 72, 24, 24)
  val dagger: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(72, 96, 24, 24)
  val sword: Graphic[Material.Bitmap] =
    Graphic(24, 24, GameAssets.assets.game.simpleSprites24x24Material).withCrop(96, 96, 24, 24)
