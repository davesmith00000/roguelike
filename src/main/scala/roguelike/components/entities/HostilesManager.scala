package roguelike.components.entities

import indigo.*
import indigo.scenes.SceneContext
import indigo.syntax.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.components.Component
import roguelike.model.GameMap
import roguelike.model.GameState
import roguelike.model.HostilesPool
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.entity.Hostile
import roguelike.viewmodel.ActorPosition
import roguelike.viewmodel.GameViewModel

object HostilesManager extends Component[Size, Model, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = HostilesM
  type ComponentViewModel = HostilesVM

  def modelLens: Lens[Model, HostilesM] =
    Lens(
      model =>
        HostilesM(
          model.gameState,
          model.hostiles,
          model.player.position,
          model.gameMap.visible
        ),
      (m, hp) => m.copy(hostiles = hp.pool)
    )

  def viewModelLens: Lens[GameViewModel, HostilesVM] =
    Lens(
      viewModel =>
        HostilesVM(
          viewModel.tilePositions,
          viewModel.squareSize,
          viewModel.hostilePositions
        ),
      (viewModel, hvm) =>
        viewModel.copy(hostilePositions = hvm.hostilePositions)
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
          .map(es => model.copy(pool = model.pool.copy(done = es)))

  private def updateNextHostile(
      context: SceneContext[Size],
      model: HostilesM,
      gameMap: GameMap
  ): (Hostile, Batch[Hostile]) => Outcome[Hostile] = (h, hs) =>
    val randomDirection: () => Point =
      () =>
        HostilesPool.getRandomDirection(
          context.dice,
          h.position,
          gameMap
        )

    val f: Hostile => Batch[Point] =
      h => if h.blocksMovement then Batch(h.position) else Batch.empty

    val entityPositions =
      hs.flatMap(f)

    val getPathTo: (Point, Point) => Batch[Point] =
      (from, to) => gameMap.getPathTo(from, to, entityPositions)

    HostileComponent.updateModel(
      context,
      h,
      HostileComponent.Cmds
        .Update(model.playerPosition, randomDirection, getPathTo)
    )

  def nextModel(
      context: SceneContext[Size],
      model: HostilesM
  ): Cmds => Outcome[HostilesM] =
    case Cmds.Update(gameMap) =>
      model.pool
        .update(
          model.visibleTiles,
          updateNextHostile(context, model, gameMap)
        )
        .map { nextPool =>
          model.copy(pool = nextPool)
        }

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

    case Cmds.CompleteInProgressHostile =>
      model.pool.completeInProgress
        .map { nextPool =>
          model.copy(pool = nextPool)
        }

  def nextViewModel(
      context: SceneContext[Size],
      model: HostilesM,
      viewModel: HostilesVM
  ): Cmds => Outcome[HostilesVM] =
    case Cmds.Update(_) =>
      model.gameState match
        case GameState.UpdateNPCs =>
          model.pool.inProgress match
            case None =>
              Outcome(viewModel)

            case Some(hostile) =>
              HostileComponent
                .updateViewModel(
                  context,
                  hostile,
                  viewModel,
                  HostileComponent.Cmds.UpdateViewModel
                )

        case _ =>
          Outcome(viewModel)

    case Cmds.CompleteInProgressHostile =>
      Outcome(viewModel)

    case _ =>
      Outcome(viewModel)

  def view(
      context: SceneContext[Size],
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
          HostileComponent
            .HostileVM(viewModel.squareSize, viewModel.hostilePositions)
        )
      }

  enum Cmds:
    case ConfuseHostile(playerName: String, id: Int, numberOfTurns: Int)
    case AttackHostileMelee(playerName: String, id: Int, attackPower: Int)
    case AttackHostileRanged(playerName: String, id: Int, damage: Int)
    case Update(gameMap: GameMap)
    case CompleteInProgressHostile

  final case class HostilesM(
      gameState: GameState,
      pool: HostilesPool,
      playerPosition: Point,
      visibleTiles: Batch[Point]
  )
  final case class HostilesVM(
      tilePositions: Batch[Point],
      squareSize: Point,
      hostilePositions: Map[Int, ActorPosition]
  )
