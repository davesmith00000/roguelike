package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.components.Component
import roguelike.model.HostilesPool
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.entity.Hostile
import roguelike.viewmodel.GameViewModel

object HostilesManager extends Component[Size, Model, GameViewModel]:
  type Command            = Cmds
  type ComponentModel     = HostilesM
  type ComponentViewModel = HostilesVM

  def modelLens: Lens[Model, HostilesM] =
    Lens(
      model => HostilesM(model.hostiles, model.gameMap.visible),
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

  final case class HostilesM(pool: HostilesPool, visibleTiles: Batch[Point])
  final case class HostilesVM(tilePositions: Batch[Point], squareSize: Point)
