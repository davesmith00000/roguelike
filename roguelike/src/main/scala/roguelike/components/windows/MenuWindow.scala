package roguelike.components.windows

import indigo.*
import roguelike.model.GameWindowContext
import roguelikestarterkit.*

object MenuWindow:

  private val graphic = Graphic(0, 0, TerminalMaterial(AssetName(""), RGBA.White, RGBA.Black))
  private val saveKey = "save1"

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[Size, GameWindowContext, MainMenu] =
    WindowModel(
      WindowId("Menu"),
      charSheet,
      MainMenu(
        ComponentGroup(Bounds(0, 0, 16, 1))
          .withLayout(ComponentLayout.Horizontal(Padding.right(1)))
          .add(
            Batch(
              "Character",
              "Inventory",
              "Drop",
              "Equip",
              "History",
              "Controls",
              "Quit"
            ).map { label =>
              Button(Bounds(0, 0, label.length(), 1))(
                presentButton(charSheet, label, RGBA.Silver, RGBA.Black)
              )
                // .onClick(PaintEvent.Load(saveKey))
                .presentOver(presentButton(charSheet, label, RGBA.White, RGBA.Black))
                .presentDown(presentButton(charSheet, label, RGBA.Black, RGBA.White))
            }
          )
      )
    )
      .moveTo(0, 0)
      .resizeTo(
        Dimensions(screenSize.width, 3)
      )
      .isStatic
      .updateModel(updateModel)
      .present(present)

  def updateModel(
      context: UiContext[Size, GameWindowContext],
      model: MainMenu
  ): GlobalEvent => Outcome[MainMenu] =
    case e =>
      model.components.update(context)(e).map { c =>
        model.copy(components = c)
      }

  def present(
      context: UiContext[Size, GameWindowContext],
      model: MainMenu
  ): Outcome[SceneUpdateFragment] =
    model.components.present(context).map { c =>
      SceneUpdateFragment(
        Layer(
          c.nodes
        ).addNodes(
          Text(
            s"My Generic Roguelike",
            RoguelikeTiles.Size10x10.Fonts.fontInfo.fontKey,
            TerminalMaterial(context.charSheet.assetName, RGBA.White, RGBA.Zero)
          ).moveTo(context.screenSpaceBounds.horizontalCenter, context.screenSpaceBounds.position.y)
        )
      )
        .addCloneBlanks(c.cloneBlanks)
    }

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

final case class MainMenu(components: ComponentGroup)
