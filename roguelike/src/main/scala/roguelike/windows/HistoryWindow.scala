package roguelike.windows

import indigo.*
import roguelike.GameEvent
import roguelike.model.GameWindowContext
import roguelikestarterkit.*

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
      .isCloseable

  given WindowContent[TerminalClones, GameWindowContext] with

    def updateModel(
        context: UIContext[GameWindowContext],
        model: TerminalClones
    ): GlobalEvent => Outcome[TerminalClones] =
      case GameEvent.RedrawHistoryLog =>
        val tcs =
          context.reference.messageLog
            .toTerminal(context.bounds.dimensions.unsafeToSize, false, 0, false)
            .toCloneTiles(
              CloneId("history clones"),
              Point.zero,
              context.charSheet.charCrops
            ) { (fg, bg) =>
              Graphic(10, 10, TerminalMaterial(context.charSheet.assetName, fg, bg))
            }

        Outcome(tcs)

      case _ =>
        Outcome(model)

    def present(
        context: UIContext[GameWindowContext],
        model: TerminalClones
    ): Outcome[Layer] =
      Outcome(
        Layer.Content(
          Batch(
            Group(model.clones)
              .moveTo(context.screenSpaceBounds.position)
          ),
          Batch.empty,
          None,
          None,
          None,
          None,
          model.blanks,
          None
        )
      )

    def refresh(reference: GameWindowContext, model: TerminalClones, contentDimensions: Dimensions): TerminalClones =
      model
