package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.components.Component
import roguelike.model.HostilesPool
import roguelike.model.entity.Hostile
import roguelike.model.entity.Orc
import roguelike.model.entity.Troll
import roguelike.viewmodel.GameViewModel

object HostileComponent extends Component[Size, Hostile, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = Hostile
  type ComponentViewModel = HostileVM

  def modelLens: Lens[Hostile, Hostile] = Lens.keepLatest

  def viewModelLens: Lens[GameViewModel, HostileVM] =
    Lens.readOnly(viewModel => HostileVM(viewModel.squareSize))

  def nextModel(
      context: FrameContext[Size],
      hostile: Hostile
  ): Cmds => Outcome[Hostile] =
    case Cmds.ConfuseFor(numOfTurns) =>
      hostile.confuseFor(numOfTurns)

  def nextViewModel(
      context: FrameContext[Size],
      hostile: Hostile,
      viewModel: HostileVM
  ): Cmds => Outcome[HostileVM] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      hostile: Hostile,
      viewModel: HostileVM
  ): Batch[SceneNode] =
    val color =
      hostile match
        case h: Orc if h.isAlive   => RGBA(0.2, 0.8, 0.0, 1.0)
        case _: Orc                => RGBA(1.0, 0.1, 0.1, 0.75)
        case h: Troll if h.isAlive => RGBA(0.3, 0.6, 0.0, 1.0)
        case _: Troll              => RGBA(0.1, 0.2, 1.0, 0.75)

    val size: Int =
      hostile match
        case h: Orc if h.isAlive   => (viewModel.squareSize.x * 0.25).toInt
        case _: Orc                => (viewModel.squareSize.x * 0.35).toInt
        case h: Troll if h.isAlive => (viewModel.squareSize.x * 0.4).toInt
        case _: Troll              => (viewModel.squareSize.x * 0.5).toInt

    val position = hostile.position * viewModel.squareSize

    val halfSquareWidth = viewModel.squareSize.x / 2

    val healthbar: Batch[SceneNode] =
      if !hostile.isAlive then Batch.empty
      else
        Batch(
          Shape.Box(
            Rectangle(
              position + Point(halfSquareWidth / 2, -6),
              Size(halfSquareWidth, 5)
            ),
            Fill.Color(RGBA.Red),
            Stroke(1, RGBA.Black)
          ),
          Shape.Box(
            Rectangle(
              position + Point(halfSquareWidth / 2, -6),
              Size(
                (halfSquareWidth * hostile.fighter.hpAsMultiplier).toInt,
                5
              )
            ),
            Fill.Color(RGBA.Green),
            Stroke(1, RGBA.Black)
          )
        )

    Shape.Circle(
      position + halfSquareWidth,
      size,
      Fill.Color(color),
      Stroke(2, color.mix(RGBA.Black, 0.5))
    ) :: healthbar

  enum Cmds:
    case ConfuseFor(numberOfTurns: Int)

  final case class HostileVM(squareSize: Point)
