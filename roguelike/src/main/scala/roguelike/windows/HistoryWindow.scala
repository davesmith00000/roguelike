package roguelike.windows

import indigo._
import roguelike.GameEvent
import roguelike.model.GameWindowContext
import roguelikestarterkit._

object HistoryWindow:

  val windowId: WindowId = WindowId("History")

  def window(
      charSheet: CharSheet
  ): WindowModel[TerminalClones, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      TerminalClones.empty
    )
      .withTitle("History")
      .moveTo(0, 10)
      .resizeTo(
        Dimensions(25, 10)
      )
      .isDraggable
      .isResizable

  given WindowContent[TerminalClones, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: TerminalClones
    ): GlobalEvent => Outcome[TerminalClones] =
      case GameEvent.RedrawHistoryLog =>
        val tcs =
          context.reference.messageLog
            .toTerminal(context.bounds.dimensions.unsafeToSize, false, 0, false)
            .toCloneTiles(
              CloneId("history clones"),
              context.screenSpaceBounds.position,
              context.charSheet.charCrops
            ) { (fg, bg) =>
              Graphic(10, 10, TerminalMaterial(context.charSheet.assetName, fg, bg))
            }

        Outcome(tcs)

      case _ =>
        Outcome(model)

    def present(
        context: UiContext[GameWindowContext],
        model: TerminalClones
    ): Outcome[Layer] =
      Outcome(model.toLayer)

    def cascade(model: TerminalClones, newBounds: Bounds): TerminalClones =
      model
