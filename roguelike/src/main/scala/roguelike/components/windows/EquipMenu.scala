package roguelike.components.windows

import indigo.*
import indigo.scenes.SceneContext
import roguelike.GameEvent
import roguelike.assets.GameAssets
import roguelike.components.Component
import roguelike.game.UIElements
import roguelike.model.Inventory
import roguelike.model.Model
import roguelike.viewmodel.GameViewModel
import roguelikestarterkit.*

object EquipMenu extends Component[Size, Model, GameViewModel]:
  type Command            = HandleInput
  type ComponentModel     = Inventory
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Inventory] =
    Lens(
      _.player.inventory,
      (model, i) => model.copy(player = model.player.copy(inventory = i))
    )

  def viewModelLens: Lens[GameViewModel, Size] = Lens.readOnly(_.viewportSize)

  def nextModel(
      context: SceneContext[Size],
      model: Inventory
  ): HandleInput => Outcome[Inventory] =
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
      context: SceneContext[Size],
      model: Inventory,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      model: Inventory,
      viewportSize: Size
  ): Outcome[Batch[SceneNode]] =
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

    Outcome(
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
            TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)
      )
    )

  final case class HandleInput(key: Key)
