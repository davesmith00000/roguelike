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

object InventoryMenu extends Component[Model, Size]:
  type Command            = HandleInput
  type ComponentModel     = Inventory
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Inventory] =
    Lens(
      _.player.inventory,
      (model, i) => model.copy(player = model.player.copy(inventory = i))
    )

  def viewModelLens: Lens[Size, Size] = Lens.identity

  def nextModel(inventory: Inventory): HandleInput => Outcome[Inventory] =
    case HandleInput(key) =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(inventory)

        case Some(keyIndex) =>
          inventory
            .use(keyIndex)
            .addGlobalEvents(
              GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
            )

  def nextViewModel(
      inventory: Inventory,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      inventory: Inventory,
      viewportSize: Size
  ): Batch[SceneNode] =
    val windowSize = Size(350, 200)

    if inventory.backpack.items.length > 0 then
      val items: String =
        inventory.backpack.items
          .map { case item =>
            item.name
          }
          .zip(UIElements.letters)
          .foldLeft(("", 0)) { case ((str, r), (collectableName, letter)) =>
            (str + s"[$letter] $collectableName\n", r + 1)
          }
          ._1

      val text =
        "Select an item to use\n" + items

      Batch(
        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Green)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)
      )
    else
      val text =
        s"""Select an item to use
          |
          |(Empty)
          |""".stripMargin

      Batch(
        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Green)
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
