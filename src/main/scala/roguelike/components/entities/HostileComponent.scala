package roguelike.components.entities

import indigo.*
import indigo.scenes.SceneContext
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
import roguelike.viewmodel.ActorPosition

object HostileComponent
    extends Component[Size, Hostile, HostilesManager.HostilesVM]:
  type Command            = Cmds
  type ComponentModel     = Hostile
  type ComponentViewModel = HostileVM

  def modelLens: Lens[Hostile, Hostile] = Lens.keepLatest

  def viewModelLens: Lens[HostilesManager.HostilesVM, HostileVM] =
    Lens(
      viewModel => HostileVM(viewModel.squareSize, viewModel.hostilePositions),
      (viewModel, hvm) =>
        viewModel.copy(hostilePositions = hvm.hostilePositions)
    )

  def nextModel(
      context: SceneContext[Size],
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
          val path = getPathTo(x.position, playerPosition)

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

    case Cmds.UpdateViewModel =>
      Outcome(hostile)

  def nextViewModel(
      context: SceneContext[Size],
      hostile: Hostile,
      viewModel: HostileVM
  ): Cmds => Outcome[HostileVM] =
    case Cmds.UpdateViewModel =>
      viewModel.hostilePositions.get(hostile.id) match
        case None =>
          Outcome(
            viewModel.copy(
              hostilePositions = viewModel.hostilePositions +
                (hostile.id -> ActorPosition(hostile.position))
            )
          )

        case Some(hp) =>
          hp.next(
            context.delta,
            hostile.position,
            GameEvent.NPCMoveComplete
          ).map { pp =>
            viewModel.copy(
              hostilePositions = viewModel.hostilePositions + (hostile.id -> pp)
            )
          }

    case _ =>
      Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      hostile: Hostile,
      viewModel: HostileVM
  ): Batch[SceneNode] =
    val color =
      hostile match
        case h: Orc if h.isAlive   => RGBA(0.2, 0.8, 0.0, 1.0)
        case _: Orc                => RGBA(1.0, 0.1, 0.1, 0.75)
        case h: Troll if h.isAlive => RGBA(0.3, 0.6, 0.0, 1.0)
        case _: Troll              => RGBA(0.1, 0.2, 1.0, 0.75)

    val radius: Double =
      hostile match
        case h: Orc if h.isAlive   => (viewModel.squareSize.x * 0.25)
        case _: Orc                => (viewModel.squareSize.x * 0.35)
        case h: Troll if h.isAlive => (viewModel.squareSize.x * 0.4)
        case _: Troll              => (viewModel.squareSize.x * 0.5)

    val size =
      viewModel.hostilePositions.get(hostile.id) match
        case None =>
          radius.toInt

        case Some(hp) =>
          (radius * (hp.attacking + 1.0)).toInt

    val position =
      viewModel.hostilePositions.get(hostile.id) match
        case None =>
          hostile.position * viewModel.squareSize

        case Some(hp) =>
          hp.moving(viewModel.squareSize)

    val halfSquareWidth = viewModel.squareSize.x / 2

    val healthbar: Batch[SceneNode] =
      val barPos = position + Point(-(halfSquareWidth / 2), -halfSquareWidth)

      if !hostile.isAlive then Batch.empty
      else
        Batch(
          Shape.Box(
            Rectangle(
              barPos,
              Size(halfSquareWidth, 5)
            ),
            Fill.Color(RGBA.Red),
            Stroke(1, RGBA.Black)
          ),
          Shape.Box(
            Rectangle(
              barPos,
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
      position,
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
        getPathTo: (Point, Point) => Batch[Point]
    )
    case UpdateViewModel

  final case class HostileVM(
      squareSize: Point,
      hostilePositions: Map[Int, ActorPosition]
  )
