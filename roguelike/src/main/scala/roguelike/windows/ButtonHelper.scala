package roguelike.windows

import indigo.*
import roguelikestarterkit.*

object ButtonHelper:

  private val graphic = Graphic(0, 0, TerminalMaterial(AssetName(""), RGBA.White, RGBA.Black))

  def makeButton(charSheet: CharSheet, label: String, clickEvents: Batch[GlobalEvent]): Button =
    Button(Bounds(0, 0, label.length, 1))(
      presentButton(charSheet, label, RGBA.Silver, RGBA.Black)
    )
      .onClick(clickEvents)
      .presentOver(presentButton(charSheet, label, RGBA.White, RGBA.Black))
      .presentDown(presentButton(charSheet, label, RGBA.Black, RGBA.White))

  def presentButton(
      charSheet: CharSheet,
      text: String,
      fgColor: RGBA,
      bgColor: RGBA
  ): (Coords, Bounds) => Outcome[ComponentFragment] =
    (offset, bounds) =>
      val size = bounds.dimensions.unsafeToSize

      val terminal =
        RogueTerminalEmulator(size)
          .putLine(Point.zero, text, fgColor, bgColor)
          .toCloneTiles(
            CloneId("button"),
            bounds.coords
              .toScreenSpace(charSheet.size)
              .moveBy(offset.toScreenSpace(charSheet.size)),
            charSheet.charCrops
          ) { case (fg, bg) =>
            graphic.withMaterial(TerminalMaterial(charSheet.assetName, fg, bg))
          }

      Outcome(
        ComponentFragment(
          terminal.clones
        ).addCloneBlanks(terminal.blanks)
      )
