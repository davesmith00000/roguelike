package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.HostileEvent
import roguelike.components.Component
import roguelike.components.windows.WindowManagerCommand
import roguelike.model.GameMap
import roguelike.model.HostilesPool
import roguelike.model.Message
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
    case Cmds.Update(playerPosition, randomDirection, getPathTo) =>
      hostile match
        case x if playerPosition.distanceTo(x.position) <= 1 =>
          // Close enough to attack!
          val event = GameEvent.Hostile(
            HostileEvent.HostileMeleeAttack(x.name, x.fighter.power)
          )
          Outcome(x, Batch(event))

        case x if x.isConfused =>
          // Is confused!
          Outcome(x.nextState.moveTo(randomDirection()))

        case x =>
          // Otherwise, move a little closer...
          val path = getPathTo(context.dice, x.position, playerPosition)

          // First path result is current location, we want the next one if it exists.
          path.drop(1).headOption match
            case Some(nextPosition) =>
              Outcome(x.moveTo(nextPosition))

            case None =>
              Outcome(x)

    case Cmds.ConfuseFor(numOfTurns) =>
      hostile.confuseFor(numOfTurns)

    case Cmds.DamageBy(amount) =>
      hostile.takeDamage(amount)

    case Cmds.TakeDamage(attackerName, attackPower) =>
      val damage = Math.max(0, attackPower - hostile.fighter.defense)

      val msg =
        if damage > 0 then
          Message(
            s"${attackerName.capitalize} attacks for $damage hit points.",
            ColorScheme.playerAttack
          )
        else
          Message(
            s"${attackerName.capitalize} attacks but does no damage",
            ColorScheme.playerAttack
          )

      val res = hostile.takeDamage(damage)

      val events =
        Batch(
          GameEvent.WindowEvent(WindowManagerCommand.CloseAll),
          GameEvent.Log(msg)
        ) ++
          res.globalEventsOrNil.reverse

      res.clearGlobalEvents
        .addGlobalEvents(events)

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
    case DamageBy(amount: Int)
    case TakeDamage(attackerName: String, attackPower: Int)
    case Update(
        playerPosition: Point,
        randomDirection: () => Point,
        getPathTo: (Dice, Point, Point) => Batch[Point]
    )

  final case class HostileVM(squareSize: Point)
