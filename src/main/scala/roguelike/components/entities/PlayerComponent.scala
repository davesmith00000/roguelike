package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.GameEvent
import roguelike.ViewModelEvent
import roguelike.components.Component
import roguelike.model.Model
import roguelike.model.entity.Player
import roguelike.viewmodel.ActorPosition
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.GameViewModelPhase

object PlayerComponent extends Component[Size, Model, GameViewModel]:
  type ComponentModel     = Player
  type ComponentViewModel = PlayerVM
  type Command            = Unit

  def modelLens: Lens[Model, Player] =
    Lens(
      _.player,
      (m, p) => m.copy(player = p)
    )

  def viewModelLens: Lens[GameViewModel, PlayerVM] =
    Lens(
      vm => PlayerVM(vm.playerPosition, vm.squareSize, vm.phase),
      (vm, p) => vm.copy(playerPosition = p.playerPosition)
    )

  def nextModel(
      context: FrameContext[Size],
      model: Player
  ): Unit => Outcome[Player] =
    _ => Outcome(model)

  def nextViewModel(
      context: FrameContext[Size],
      model: Player,
      viewModel: PlayerVM
  ): Unit => Outcome[PlayerVM] =
    _ =>
      viewModel.phase match
        case GameViewModelPhase.MovingPlayer =>
          viewModel.playerPosition
            .next(
              context.delta,
              model.position,
              GameEvent.ViewModelPhaseComplete(
                ViewModelEvent.PlayerMoveComplete
              )
            )
            .map { pp =>
              viewModel.copy(
                playerPosition = pp
              )
            }

        case GameViewModelPhase.Idle =>
          Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      model: Player,
      viewModel: PlayerVM
  ): Batch[SceneNode] =
    Batch(
      Shape.Circle(
        viewModel.playerPosition.display,
        (viewModel.squareSize.x / 2.5).toInt,
        Fill.Color(RGBA.White),
        Stroke(2, RGBA.Black)
      )
    )

  final case class PlayerVM(
      playerPosition: ActorPosition,
      squareSize: Point,
      phase: GameViewModelPhase
  )
