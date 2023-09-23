package roguelike.components.windows

import indigo.*
import indigo.scenes.SceneContext
import io.indigoengine.roguelike.starterkit.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.assets.GameAssets
import roguelike.assets.GameAssetsNew
import roguelike.components.Component
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.entity.Player
import roguelike.viewmodel.GameViewModel

object LevelUp extends Component[Size, Model, GameViewModel]:
  type Command            = HandleInput
  type ComponentModel     = Player
  type ComponentViewModel = Size

  def modelLens: Lens[Model, Player] =
    Lens(
      _.player,
      (m, p) => m.copy(player = p)
    )

  def viewModelLens: Lens[GameViewModel, Size] = Lens.readOnly(_.viewportSize)

  def nextModel(
      context: SceneContext[Size],
      player: Player
  ): HandleInput => Outcome[Player] =
    // Constitution
    case HandleInput(Key.KEY_1) =>
      player
        .increaseMaxHp(20)
        .addGlobalEvents(
          GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
        )

    // Strength
    case HandleInput(Key.KEY_2) =>
      player
        .increasePower(1)
        .addGlobalEvents(
          GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
        )

    // Agility
    case HandleInput(Key.KEY_3) =>
      player
        .increaseDefense(1)
        .addGlobalEvents(
          GameEvent.WindowEvent(WindowManagerCommand.CloseAll)
        )

    case HandleInput(_) =>
      Outcome(player).addGlobalEvents(
        GameEvent.Log(
          Message("Invalid, please press 1, 2, or 3.", ColorScheme.invalid)
        )
      )

  def nextViewModel(
      context: SceneContext[Size],
      model: Player,
      viewModel: Size
  ): HandleInput => Outcome[Size] =
    _ => Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      player: Player,
      viewportSize: Size
  ): Outcome[Batch[SceneNode]] =
    val text =
      s"""Level Up!
        |
        |Congratulations! You level up!
        |Select an attribute to increase.
        |[1] Constitution (+20 HP, from ${player.fighter.maxHp})
        |[2] Strength (+1 attack, from ${player.fighter.power})
        |[3] Agility (+1 defense, from ${player.fighter.defense})
        |""".stripMargin

    val windowSize = Size(350, 100)

    Outcome(
      Batch(
        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Magenta)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(GameAssetsNew.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)
      )
    )

  final case class HandleInput(key: Key)
