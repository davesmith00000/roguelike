package roguelike.windows

import indigo.*
import roguelike.GameEvent
import roguelike.model.GameWindowContext
import roguelikestarterkit.*
import roguelikestarterkit.syntax.*

object LevelUpWindow:

  val windowId: WindowId = WindowId("Level up")

  private val windowSize = Dimensions(40, 10)

  def window(
      screenSize: Dimensions,
      charSheet: CharSheet
  ): WindowModel[LevelUpMenu, GameWindowContext] =
    WindowModel(
      windowId,
      charSheet,
      LevelUpMenu.initial(charSheet)
    )
      .withTitle("Level Up!")
      .moveTo(((screenSize - windowSize) / 2).toCoords)
      .resizeTo(windowSize)

final case class LevelUpMenu(components: ComponentGroup[GameWindowContext])
object LevelUpMenu:

  def initial(charSheet: CharSheet): LevelUpMenu =
    val buttonTheme =
      Button.Theme(
        charSheet,
        RGBA.Silver -> RGBA.Black,
        RGBA.White  -> RGBA.Black,
        RGBA.Black  -> RGBA.White
      )

    LevelUpMenu(
      ComponentGroup(BoundsType.fixed(16, 1))
        .withLayout(ComponentLayout.Vertical())
        .add(
          Label("Congratulations! You level up!", Label.Theme(charSheet)),
          Label("Select an attribute to increase.", Label.Theme(charSheet)),
          Label("", Label.Theme(charSheet))
        )
        .add(
          Button(
            (ctx: GameWindowContext) =>
              s"[1] Constitution (+20 HP, from ${ctx.player.fighter.maxHp})",
            buttonTheme
          ).onClick(
            GameEvent.ModifyMaxHp(20),
            GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
          ),
          Button(
            (ctx: GameWindowContext) =>
              s"[2] Strength (+1 attack, from ${ctx.player.fighter.power})",
            buttonTheme
          ).onClick(
            GameEvent.ModifyPower(1),
            GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
          ),
          Button(
            (ctx: GameWindowContext) =>
              s"[3] Agility (+1 defense, from ${ctx.player.fighter.defense})",
            buttonTheme
          ).onClick(
            GameEvent.ModifyDefense(1),
            GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
          )
        )
    )

  given (using
      cg: WindowContent[ComponentGroup[GameWindowContext], GameWindowContext]
  ): WindowContent[LevelUpMenu, GameWindowContext] with

    def updateModel(
        context: UIContext[GameWindowContext],
        model: LevelUpMenu
    ): GlobalEvent => Outcome[LevelUpMenu] =

      // Constitution
      case KeyboardEvent.KeyUp(Key.KEY_1) =>
        Outcome(
          model,
          Batch(GameEvent.ModifyMaxHp(20), GameEvent.WindowEvent(WindowManagerCommand.CloseAll))
        )

      // Strength
      case KeyboardEvent.KeyUp(Key.KEY_2) =>
        Outcome(
          model,
          Batch(GameEvent.ModifyPower(1), GameEvent.WindowEvent(WindowManagerCommand.CloseAll))
        )

      // Agility
      case KeyboardEvent.KeyUp(Key.KEY_3) =>
        Outcome(
          model,
          Batch(GameEvent.ModifyDefense(1), GameEvent.WindowEvent(WindowManagerCommand.CloseAll))
        )

      case e =>
        cg.updateModel(context, model.components)(e).map { c =>
          model.copy(components = c)
        }

    def present(
        context: UIContext[GameWindowContext],
        model: LevelUpMenu
    ): Outcome[Layer] =
      cg.present(context, model.components)

    def refresh(reference: GameWindowContext, model: LevelUpMenu, contentDimensions: Dimensions): LevelUpMenu =
      model.copy(components = model.components.refresh(reference, contentDimensions))
