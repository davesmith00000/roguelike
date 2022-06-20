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
  type ComponentModel     = HostilesPool
  type ComponentViewModel = HostilesVM

  def modelLens: Lens[Model, HostilesPool] =
    Lens(
      _.hostiles,
      (m, hp) => m.copy(hostiles = hp)
    )

  def viewModelLens: Lens[GameViewModel, HostilesVM] =
    Lens(
      viewModel => HostilesVM(),
      (viewModel, hostileVM) => viewModel
    )

  def updateHostile(id: Int, hostilesPool: HostilesPool)(
      modify: Hostile => Outcome[Hostile]
  )(default: => Outcome[HostilesPool]): Outcome[HostilesPool] =
    hostilesPool.findAliveById(id) match
      case None => default
      case Some(hostile) =>
        Outcome
          .sequence(
            hostilesPool.hostiles.map {
              case h if h.id == id && h.isAlive => modify(h)
              case h                            => Outcome(h)
            }
          )
          .map(es => hostilesPool.copy(hostiles = es))

  def nextModel(
      context: FrameContext[Size],
      hostilesPool: HostilesPool
  ): Cmds => Outcome[HostilesPool] =
    case Cmds.ConfuseHostile(playerName, id, numberOfTurns) =>
      updateHostile(id, hostilesPool) { hostile =>
        HostileComponent
          .updateModel(
            context,
            hostile,
            HostileComponent.Cmds.ConfuseFor(numberOfTurns)
          )
          .addGlobalEvents(GameEvent.PlayerTurnEnd)
      } {
        Outcome(hostilesPool)
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
      hostilesPool: HostilesPool,
      viewModel: HostilesVM
  ): Cmds => Outcome[HostilesVM] =
    _ => Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      hostilesPool: HostilesPool,
      viewModel: HostilesVM
  ): Batch[SceneNode] =
    Batch()

  enum Cmds:
    case ConfuseHostile(playerName: String, id: Int, numberOfTurns: Int)

  final case class HostilesVM()
