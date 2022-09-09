package roguelike

import indigo._

object Assets:

  object Basic:
    val indigoLogo         = AssetName("indigoLogo")
    val purpleKingdomLogo  = AssetName("pkLogo")
    val textFragShader     = AssetName("text frag")
    val tileMap: AssetName = AssetName("Anikki_square_10x10")

    val assets: Set[AssetType] =
      Set(
        AssetType.Text(textFragShader, AssetPath("assets/shaders/text.frag")),
        AssetType
          .Image(tileMap, AssetPath("assets/" + tileMap.toString + ".png")),
        AssetType
          .Image(
            purpleKingdomLogo,
            AssetPath("assets/" + purpleKingdomLogo.toString + ".png")
          ),
        AssetType
          .Image(
            indigoLogo,
            AssetPath("assets/" + indigoLogo.toString + ".png")
          )
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

object GameGraphics:

  val tileClone: CloneBlank =
    CloneBlank(
      CloneId("rogue bg tiles"),
      Graphic(24, 24, Material.Bitmap(Assets.Game.icons))
    )

  // Bg tiles
  val wall: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(0, 0, 24, 24)
  val wallInShadow: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(24, 0, 24, 24)

  val floor: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(0, 24, 24, 24)
  val floorInShadow: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(24, 24, 24, 24)

  val stairs: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(48, 0, 24, 24)
  val stairsInShadow: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(48, 24, 24, 24)

  def wallTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 0, 0, 24, 24)
  def wallInShadowTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 24, 0, 24, 24)

  def floorTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 0, 24, 24, 24)
  def floorInShadowTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 24, 24, 24, 24)

  def stairsTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 48, 0, 24, 24)
  def stairsInShadowTile(position: Point): CloneTileData =
    CloneTileData(position.x, position.y, 48, 24, 24, 24)

  //

  val highlight: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(0, 48, 24, 24)

  val target: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(24, 48, 24, 24)

  val fireScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(0, 72, 24, 24)
  val lightningScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(24, 72, 24, 24)
  val confusionScroll: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(0, 96, 24, 24)
  val scrollInShadow: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(24, 96, 24, 24)

  val healthPotion: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(48, 96, 24, 24)

  val leatherArmour: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(72, 72, 24, 24)
  val chainmail: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(96, 72, 24, 24)
  val dagger: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(72, 96, 24, 24)
  val sword: Graphic[Material.Bitmap] =
    Graphic(24, 24, Material.Bitmap(Assets.Game.icons)).withCrop(96, 96, 24, 24)
