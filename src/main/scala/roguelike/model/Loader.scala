package roguelike.model

import indigo.*
import indigo.scenes.SceneContext
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets

final case class Loader(context: SceneContext[Size], state: LoadingState):
  val paddingToText = 72
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

        case LoadingState.Error =>
          "Error, some assets could not be loaded."
      ,
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.TileMap, RGB.White, RGBA.Zero)
    )

  def getBounds() =
    val textBounds = context.boundaryLocator.textBounds(text)
    Rectangle(textBounds.width, paddingToText + textBounds.height)

  def view() =
    val graphic =
      Clip(
        Point(0),
        Size(32),
        ClipSheet(4, Seconds(0.25)),
        Material.Bitmap(GameAssets.Loader)
      )

    val textBounds = context.boundaryLocator.textBounds(text)
    val midX       = textBounds.width / 2

    Group(
      graphic.moveTo((midX - 16).toInt, 0),
      text.moveTo(0, paddingToText)
    )
