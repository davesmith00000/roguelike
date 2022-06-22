package roguelike.components.entities

import indigo.*
import indigo.scenes.Lens
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.HostileEvent
import roguelike.ViewModelEvent
import roguelike.components.Component
import roguelike.components.windows.WindowManagerCommand
import roguelike.model.Inventory
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.entity.Player
import roguelike.model.gamedata.Consumables
import roguelike.model.items.Item
import roguelike.viewmodel.ActorPosition
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.GameViewModelPhase

object PlayerComponent extends Component[Size, Model, GameViewModel]:
  type ComponentModel     = Player
  type ComponentViewModel = PlayerVM
  type Command            = Cmds

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

  val inventoryLens: Lens[Player, Inventory] =
    Lens(
      _.inventory,
      (p, i) => p.copy(inventory = i)
    )

  def nextModel(
      context: FrameContext[Size],
      player: Player
  ): Cmds => Outcome[Player] =
    case Cmds.RemoveFromInvetory(at) =>
      Outcome(inventoryLens.modify(player, _.remove(at)))

    case Cmds.GiveToPlayer(i) =>
      Outcome(inventoryLens.modify(player, _.add(i)))

    case Cmds.UseConsumable(h: Consumables.HealthPotion.type) =>
      val possibleAmount  = player.fighter.maxHp - player.fighter.hp
      val amountRecovered = Math.min(possibleAmount, h.amount)

      if amountRecovered <= 0 then
        Outcome(
          inventoryLens.modify(player, _.add(h)),
          Batch(
            GameEvent.Log(
              Message("Your health is already full.", ColorScheme.impossible)
            )
          )
        )
      else
        Outcome(
          player.heal(amountRecovered),
          Batch(
            GameEvent.Log(
              Message(
                s"You consume the ${h.name}, and recover $amountRecovered",
                ColorScheme.healthRecovered
              )
            ),
            GameEvent.PlayerTurnEnd
          )
        )

    case Cmds.HostileInteraction(e) =>
      handleHostileEvent(context, player)(e)

    case Cmds.Update =>
      Outcome(player)

  def nextViewModel(
      context: FrameContext[Size],
      player: Player,
      viewModel: PlayerVM
  ): Cmds => Outcome[PlayerVM] =
    case Cmds.Update =>
      viewModel.phase match
        case GameViewModelPhase.MovingPlayer =>
          viewModel.playerPosition
            .next(
              context.delta,
              player.position,
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

    case _ =>
      Outcome(viewModel)

  def view(
      context: FrameContext[Size],
      player: Player,
      viewModel: PlayerVM
  ): Batch[SceneNode] =
    Batch(
      Shape.Circle(
        viewModel.playerPosition.display(viewModel.squareSize),
        (viewModel.squareSize.x / 2.5).toInt,
        Fill.Color(RGBA.White),
        Stroke(2, RGBA.Black)
      )
    )

  def handleHostileEvent(
      context: FrameContext[Size],
      player: Player
  ): HostileEvent => Outcome[Player] = {
    case HostileEvent.HostileMeleeAttack(name, power) =>
      val damage = Math.max(
        0,
        power - (player.fighter.defense + player.inventory.equipment.defenseBonus)
      )

      val attackMessage =
        GameEvent.Log(
          if damage > 0 then
            Message(
              s"${name.capitalize} attacks for $damage hit points.",
              ColorScheme.enemyAttack
            )
          else
            Message(
              s"${name.capitalize} attacks but does no damage",
              ColorScheme.enemyAttack
            )
        )

      val p = player.takeDamage(damage)

      val msgs =
        if p.isAlive then Batch(attackMessage)
        else
          GameEvent.Log(
            Message("You died!", ColorScheme.playerDie)
          ) :: attackMessage :: Batch.empty

      Outcome(p, msgs)

    case HostileEvent.HostileGiveXP(amount) =>
      player
        .addXp(amount)
        .createGlobalEvents(p =>
          if p.level > player.level then
            Batch(GameEvent.WindowEvent(WindowManagerCommand.ShowLevelUp))
          else Batch.empty
        )
  }

  final case class PlayerVM(
      playerPosition: ActorPosition,
      squareSize: Point,
      phase: GameViewModelPhase
  )

  enum Cmds:
    case Update
    case RemoveFromInvetory(at: Int)
    case GiveToPlayer(item: Item)
    case UseConsumable(c: Consumables)
    case HostileInteraction(e: HostileEvent)
