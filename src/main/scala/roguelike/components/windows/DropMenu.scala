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

object DropMenu extends Component[Model, Size]:
  type Command            = HandleInput
  type ComponentModel     = DropMenuModel
  type ComponentViewModel = Size

  def modelLens: Lens[Model, DropMenuModel] =
    Lens(
      model =>
        DropMenuModel(
          model.player.inventory,
          model.player.position,
          model.gameMap.collectables
        ),
      (model, drop) =>
        model.copy(player = model.player.copy(inventory = drop.inventory))
    )

  def viewModelLens: Lens[Size, Size] = Lens.identity

  def nextModel(model: DropMenuModel): HandleInput => Outcome[DropMenuModel] =
    case HandleInput(key) =>
      UIElements.letterPositions.get(key.key) match
        case None =>
          Outcome(model)

        case Some(keyIndex) =>
          if model.worldCollectables.exists(
              _.position == model.mapPosition
            ) && model.inventory.backpack.items.nonEmpty
          then
            Outcome(model)
              .addGlobalEvents(
                GameEvent.Log(Message("Cannot drop here.", ColorScheme.invalid))
              )
          else
            model.inventory
              .drop(keyIndex, model.mapPosition)
              .map(inv => model.copy(inventory = inv))
              .addGlobalEvents(
                GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
              )

  def nextViewModel(
      model: DropMenuModel,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      model: DropMenuModel,
      viewportSize: Size
  ): Batch[SceneNode] =
    val windowSize = Size(350, 200)

    if model.inventory.backpack.count > 0 then
      val collectables: String =
        model.inventory.backpack.items
          .map(_.name)
          .zip(UIElements.letters)
          .foldLeft(("", 0)) { case ((str, r), (collectableName, letter)) =>
            (str + s"[$letter] $collectableName\n", r + 1)
          }
          ._1

      val text =
        "Select an collectable to drop\n" + collectables

      Batch(
        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Red)
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
        s"""Select an collectable to drop
          |
          |(Empty)
          |""".stripMargin

      Batch(
        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Red)
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
  final case class DropMenuModel(
      inventory: Inventory,
      mapPosition: Point,
      worldCollectables: Batch[Collectable]
  )