package roguelike.windows

import indigo._
import roguelike.GameEvent
import roguelike.components.windows.WindowManagerCommand
import roguelike.model.GameWindowContext
import roguelikestarterkit._

object MenuWindow:

  val windowId: WindowId = WindowId("Menu")

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[MainMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      MainMenu.initial(charSheet)
    )
      .moveTo(0, 0)
      .resizeTo(
        Dimensions(screenSize.width, 3)
      )
      .isStatic

final case class MainMenu(components: ComponentGroup)
object MainMenu:

  private val graphic = Graphic(0, 0, TerminalMaterial(AssetName(""), RGBA.White, RGBA.Black))

  def initial(charSheet: CharSheet): MainMenu =
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
            val clickEvents =
              label match
                case "History" =>
                  Batch(GameEvent.WindowEvent(WindowManagerCommand.ShowHistory))

                case "Controls" =>
                  Batch(GameEvent.WindowEvent(WindowManagerCommand.ShowControls))

                case _ =>
                  Batch.empty

            Button(Bounds(0, 0, label.length(), 1))(
              presentButton(charSheet, label, RGBA.Silver, RGBA.Black)
            )
              .onClick(clickEvents)
              .presentOver(presentButton(charSheet, label, RGBA.White, RGBA.Black))
              .presentDown(presentButton(charSheet, label, RGBA.Black, RGBA.White))
          }
        )
    )

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

  given WindowContent[MainMenu, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: MainMenu
    ): GlobalEvent => Outcome[MainMenu] =
      case e =>
        model.components.update(context)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UiContext[GameWindowContext],
        model: MainMenu
    ): Outcome[Layer] =
      model.components.present(context).map { c =>
        Layer(
          c.nodes
        ).addNodes(
          Text(
            s"My Generic Roguelike",
            RoguelikeTiles.Size10x10.Fonts.fontInfo.fontKey,
            TerminalMaterial(context.charSheet.assetName, RGBA.White, RGBA.Zero)
          ).moveTo(
            context.screenSpaceBounds.horizontalCenter,
            context.screenSpaceBounds.position.y
          )
        ).addCloneBlanks(c.cloneBlanks)
      }

    def cascade(model: MainMenu, newBounds: Bounds): MainMenu =
      model.copy(components = model.components.cascade(newBounds))
