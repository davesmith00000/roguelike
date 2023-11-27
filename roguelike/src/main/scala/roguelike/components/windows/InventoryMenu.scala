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

object InventoryMenu extends Component[Size, Model, GameViewModel]:
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
      inventory: Inventory
  ): HandleInput => Outcome[Inventory] =
    case HandleInput(key) =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(inventory)

        case Some(keyIndex) =>
          inventory
            .use(keyIndex)

  def nextViewModel(
      context: SceneContext[Size],
      inventory: Inventory,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      inventory: Inventory,
      viewportSize: Size
  ): Outcome[Batch[SceneNode]] =
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

      Outcome(
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
              TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
            )
              .moveTo(5, 5)
          ).moveTo(((viewportSize - windowSize) / 2).toPoint)
        )
      )
    else
      val text =
        s"""Select an item to use
          |
          |(Empty)
          |""".stripMargin

      Outcome(
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
              TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
            )
              .moveTo(5, 5)
          ).moveTo(((viewportSize - windowSize) / 2).toPoint)
        )
      )

  final case class HandleInput(key: Key)
