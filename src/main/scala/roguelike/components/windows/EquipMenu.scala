package roguelike.components.windows

import indigo.*
import indigo.scenes.Lens
import indigo.scenes.SceneEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.MainMenuScene
import roguelike.components.Component
import roguelike.game.UIElements
import roguelike.model.GameLoadInfo
import roguelike.model.GameState
import roguelike.model.Inventory
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.model.entity.Collectable
import roguelike.viewmodel.GameViewModel

object EquipMenu extends Component[Model, Size]:
  type Command            = HandleInput
  type ComponentModel     = Inventory
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Inventory] =
    Lens(
      _.player.inventory,
      (model, i) => model.copy(player = model.player.copy(inventory = i))
    )

  def viewModelLens: Lens[Size, Size] = Lens.identity

  def nextModel(model: Inventory): HandleInput => Outcome[Inventory] =
    case HandleInput(key) =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(0) =>
          model.unequipArmour
            .addGlobalEvents(
              GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
            )

        case Some(1) =>
          model.unequipWeapon
            .addGlobalEvents(
              GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
            )

        case _ =>
          Outcome(model)

  def nextViewModel(
      model: Inventory,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      model: Inventory,
      viewportSize: Size
  ): Batch[SceneNode] =
    val windowSize = Size(350, 200)

    val collectables: String =
      List(
        model.equipment.armour
          .map(a => "[a] " + a.name)
          .getOrElse("[ ] --No armour equipped--"),
        model.equipment.weapon
          .map(m => "[b] " + m.name)
          .getOrElse("[ ] --No melee weapon equipped--")
      ).mkString("\n")

    val text =
      "Select an item to unequip\n" + collectables

    Batch(
      Group(
        Shape.Box(
          Rectangle(Point.zero, windowSize),
          Fill.Color(RGBA.Black),
          Stroke(2, RGBA.Cyan)
        ),
        Text(
          text,
          RoguelikeTiles.Size10x10.Fonts.fontKey,
          TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
        )
          .moveTo(5, 5)
      ).moveTo(((viewportSize - windowSize) / 2).toPoint)
    )

  final case class HandleInput(key: Key)
