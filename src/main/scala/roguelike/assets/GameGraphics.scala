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
  // TODO: Making this decision on every frame seems like a terrible idea. :-)
  def wallTile(code: Option[String], position: Point): CloneTileData =
    code match
      // walls corner top right
      case Some(c) if c == "...wx.gw." =>
        CloneTileData(position.x, position.y, 96, 64, 32, 32)

      // walls right side
      case Some(c) if c == "gw.gx.gw." =>
        CloneTileData(position.x, position.y, 96, 32, 32, 32)

      // walls corner top left
      case Some(c) if c == "....xw.wg" =>
        CloneTileData(position.x, position.y, 96, 64, 32, 32)

      // walls left side
      case Some(c) if c == ".wg.xg.wg" =>
        CloneTileData(position.x, position.y, 96, 32, 32, 32)

      // drop off
      case Some(c) if c.substring(6, 9) == "w.." =>
        CloneTileData(position.x, position.y, 192, 256, 32, 32)

      case Some(c) if c.substring(6, 9) == "..w" =>
        CloneTileData(position.x, position.y, 256, 256, 32, 32)

      case Some(c) if c.charAt(7) == '.' =>
        CloneTileData(position.x, position.y, 224, 256, 32, 32)

      // default
      case _ =>
        CloneTileData(position.x, position.y, 0, 96, 32, 32)

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
