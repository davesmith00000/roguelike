package roguelike.windows

import indigo.*
import roguelike.model.GameWindowContext
import roguelike.model.gamedata.KeyMapping
import roguelikestarterkit.*

object ControlsWindow:

  val windowId: WindowId = WindowId("Controls Window")

  private val windowSize = Dimensions(26, 22)

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[Unit, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      ()
    )
      .withTitle("Controls")
      .moveTo(((screenSize - windowSize) / 2).toCoords)
      .resizeTo(windowSize)

  given WindowContent[Unit, GameWindowContext] with

    def updateModel(
        context: UiContext[GameWindowContext],
        model: Unit
    ): GlobalEvent => Outcome[Unit] =
      case _ =>
        Outcome(model)

    def present(
        context: UiContext[GameWindowContext],
        model: Unit
    ): Outcome[Layer] =
      Outcome(
        Layer(
          renderControls(
            context.charSheet,
            context.screenSpaceBounds
          )
        )
      )

    def cascade(model: Unit, newBounds: Bounds): Unit =
      model

  private val separator         = " | "
  private val commandsAndValues = KeyMapping.helpText.map(p => (p._1, p._2.mkString(separator)))
  private val longest           = KeyMapping.longestMappings + separator.length
  private val helpControlsText =
    commandsAndValues
      .map { case (cmd, value) =>
        val v = List.fill(longest - value.length)(" ").mkString + value
        s"$cmd $v"
      }
      .mkString("\n")

  def renderControls(
      charSheet: CharSheet,
      contentArea: Rectangle
  ): Text[TerminalText] =
    Text(
      helpControlsText,
      charSheet.fontKey,
      TerminalText(charSheet.assetName, RGBA.White, RGBA.Zero)
    ).alignRight.moveTo(contentArea.right, contentArea.y)
