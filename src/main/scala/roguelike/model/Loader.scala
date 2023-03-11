package roguelike.model

import indigo.*
import indigo.scenes.SceneContext
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets

object Loader:

  val paddingToText = 72

  def view(context: SceneContext[Size], state: LoadingState, viewportSize: Size): Group =

    val text =
      Text(
        state match
          case LoadingState.NotStarted =>
            "Started loading..."

          case LoadingState.InProgress(percent) =>
            "Loaded..." + (percent match
              case Some(p) => s"${p.toString}%"
              case None    => ""
            )

          case LoadingState.Complete =>
            "Loaded successfully!"

          case LoadingState.Error(msg) =>
            s"Error, some assets could not be loaded.\nError: $msg"
        ,
        RoguelikeTiles.Size10x10.Fonts.fontKey,
        TerminalText(GameAssets.TileMap, RGB.White, RGBA.Zero)
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
