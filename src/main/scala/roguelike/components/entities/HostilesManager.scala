package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import indigo.syntax.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.components.Component
import roguelike.model.GameMap
import roguelike.model.HostilesPool
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.entity.Hostile
import roguelike.viewmodel.GameViewModel

import scala.annotation.tailrec

object HostilesManager extends Component[Size, Model, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = HostilesM
  type ComponentViewModel = HostilesVM

  def modelLens: Lens[Model, HostilesM] =
    Lens(
      model =>
        HostilesM(model.hostiles, model.player.position, model.gameMap.visible),
      (m, hp) => m.copy(hostiles = hp.pool)
    )

  def viewModelLens: Lens[GameViewModel, HostilesVM] =
    Lens.readOnly(viewModel =>
      HostilesVM(viewModel.tilePositions, viewModel.squareSize)
    )

  def updateHostile(id: Int, model: HostilesM)(
      modify: Hostile => Outcome[Hostile]
  )(default: => Outcome[HostilesM]): Outcome[HostilesM] =
    model.pool.findAliveById(id) match
      case None => default
      case Some(hostile) =>
        Outcome
          .sequence(
            model.pool.hostiles.map {
              case h if h.id == id && h.isAlive => modify(h)
              case h                            => Outcome(h)
            }
          )
          .map(es => model.copy(pool = model.pool.copy(hostiles = es)))

  def nextModel(
      context: FrameContext[Size],
      model: HostilesM
  ): Cmds => Outcome[HostilesM] =
    case Cmds.Update(gameMap) =>
      @tailrec
      def rec(
          remaining: List[Hostile],
          events: Batch[GameEvent],
          acc: Batch[Outcome[Hostile]]
      ): Outcome[Batch[Hostile]] =
        remaining match
          case Nil =>
            Outcome.sequence(acc).addGlobalEvents(events)

          case x :: xs
              if !x.isAlive || !model.visibleTiles.contains(x.position) =>
            // Filter out the dead and the unseen
            rec(xs, events, Outcome(x) :: acc)

          case x :: xs =>
            val randomDirection: () => Point =
              () =>
                HostilesPool.getRandomDirection(
                  context.dice,
                  x.position,
                  gameMap
                )

            val f: Hostile => Batch[Point] =
              h => if h.blocksMovement then Batch(h.position) else Batch.empty

            val entityPositions =
              xs.toBatch.flatMap(f) ++
                Outcome
                  .sequence(acc)
                  .map(_.flatMap(f))
                  .getOrElse(Batch.empty)

            val getPathTo: (Dice, Point, Point) => Batch[Point] =
              (dice, from, to) =>
                GameMap.getPathTo(dice, from, to, entityPositions, gameMap)

            val updated =
              HostileComponent.updateModel(
                context,
                x,
                HostileComponent.Cmds
                  .Update(model.playerPosition, randomDirection, getPathTo)
              )

            rec(xs, events, updated :: acc)

      val res = rec(model.pool.hostiles.toList, Batch.empty, Batch.empty)
      res.map(hs => model.copy(pool = model.pool.copy(hostiles = hs)))

    case Cmds.ConfuseHostile(playerName, id, numberOfTurns) =>
      updateHostile(id, model) { hostile =>
        HostileComponent
          .updateModel(
            context,
            hostile,
            HostileComponent.Cmds.ConfuseFor(numberOfTurns)
          )
          .addGlobalEvents(GameEvent.PlayerTurnEnd)
      } {
        Outcome(model)
          .addGlobalEvents(
            GameEvent.Log(
              Message(
                s"${playerName.capitalize} misses!",
                ColorScheme.playerAttack
              )
            ),
            GameEvent.PlayerTurnEnd
          )
      }

    case Cmds.AttackHostileRanged(playerName, id, damage) =>
      updateHostile(id, model) { hostile =>
        HostileComponent
          .updateModel(
            context,
            hostile,
            HostileComponent.Cmds.DamageBy(damage)
          )
          .addGlobalEvents(GameEvent.PlayerTurnEnd)
      } {
        Outcome(model)
          .addGlobalEvents(
            GameEvent.Log(
              Message(
                s"${playerName.capitalize} misses!",
                ColorScheme.playerAttack
              )
            ),
            GameEvent.PlayerTurnEnd
          )
      }

    case Cmds.AttackHostileMelee(playerName, id, attackPower) =>
      updateHostile(id, model) { hostile =>
        HostileComponent
          .updateModel(
            context,
            hostile,
            HostileComponent.Cmds.TakeDamage(playerName, attackPower)
          )
          .addGlobalEvents(GameEvent.PlayerTurnEnd)
      } {
        Outcome(model)
          .addGlobalEvents(
            GameEvent.Log(
              Message(
                s"${playerName.capitalize} swings and misses!",
                ColorScheme.playerAttack
              )
            ),
            GameEvent.PlayerTurnEnd
          )
      }

  def nextViewModel(
      context: FrameContext[Size],
      model: HostilesM,
      viewModel: HostilesVM
  ): Cmds => Outcome[HostilesVM] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      model: HostilesM,
      viewModel: HostilesVM
  ): Batch[SceneNode] =
    model.pool.hostiles
      .filter(h =>
        model.visibleTiles.contains(h.position) & viewModel.tilePositions
          .contains(h.position)
      )
      .sortBy(_.isAlive)
      .flatMap { hostile =>
        HostileComponent.view(
          context,
          hostile,
          HostileComponent.HostileVM(viewModel.squareSize)
        )
      }

  enum Cmds:
    case ConfuseHostile(playerName: String, id: Int, numberOfTurns: Int)
    case AttackHostileMelee(playerName: String, id: Int, attackPower: Int)
    case AttackHostileRanged(playerName: String, id: Int, damage: Int)
    case Update(gameMap: GameMap)

  final case class HostilesM(
      pool: HostilesPool,
      playerPosition: Point,
      visibleTiles: Batch[Point]
  )
  final case class HostilesVM(tilePositions: Batch[Point], squareSize: Point)
