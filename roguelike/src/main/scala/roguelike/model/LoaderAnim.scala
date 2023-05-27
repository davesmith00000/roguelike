package roguelike.model

import indigo.*
import indigo.scenes.SceneContext
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets

object LoaderAnim:

  val paddingToText = 72

  def present(context: SceneContext[Size], textCopy: String, viewportSize: Size): Group =

    val text =
      Text(
        textCopy,
        RoguelikeTiles.Size10x10.Fonts.fontKey,
        TerminalText(GameAssets.TileMap, RGBA.White, RGBA.Zero)
      )

    val textBounds: Rectangle =
      context.boundaryLocator.textBounds(text)

    val bounds: Rectangle =
      Rectangle(textBounds.width, paddingToText + textBounds.height)

    val graphic =
      Clip(
        Point(0),
        Size(32),
        ClipSheet(4, Seconds(0.25)),
        Material.Bitmap(GameAssets.Loader)
      )

    val midX       = textBounds.width * 0.5
    val screenMidX = viewportSize.width * 0.5
    val screenMidY = viewportSize.height * 0.5

    Group(
      graphic.moveTo((midX - 16).toInt, 0),
      text.moveTo(0, paddingToText)
    ).moveTo(
      (viewportSize.toPoint / 2) - (bounds.size.toPoint / 2)
    )
