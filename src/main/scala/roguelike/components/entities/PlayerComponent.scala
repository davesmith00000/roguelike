package roguelike.components.entities

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.HostileEvent
import roguelike.assets.GameAssets
import roguelike.components.Component
import roguelike.components.windows.WindowManagerCommand
import roguelike.model.GameState
import roguelike.model.Inventory
import roguelike.model.Message
import roguelike.model.Model
import roguelike.model.entity.Player
import roguelike.model.gamedata.Consumables
import roguelike.model.items.Item
import roguelike.viewmodel.ActorMoveState
import roguelike.viewmodel.ActorPosition
import roguelike.viewmodel.GameViewModel

object PlayerComponent extends Component[Size, Model, GameViewModel]:
  type ComponentModel     = PlayerM
  type ComponentViewModel = PlayerVM
  type Command            = Cmds

  def modelLens: Lens[Model, PlayerM] =
    Lens(
      m => PlayerM(m.player, m.gameState),
      (m, pm) => m.copy(player = pm.player)
    )

  def viewModelLens: Lens[GameViewModel, PlayerVM] =
    Lens(
      vm =>
        PlayerVM(
          vm.playerPosition,
          vm.squareSize,
          vm.sprites.map(_.player)
        ),
      (vm, p) => vm.copy(playerPosition = p.playerPosition)
    )

  val inventoryLens: Lens[Player, Inventory] =
    Lens(
      _.inventory,
      (p, i) => p.copy(inventory = i)
    )

  def nextModel(
      context: SceneContext[Size],
      model: PlayerM
  ): Cmds => Outcome[PlayerM] =
    case Cmds.RemoveFromInvetory(at) =>
      Outcome(model.next(inventoryLens.modify(model.player, _.remove(at))))

    case Cmds.GiveToPlayer(i) =>
      Outcome(model.next(inventoryLens.modify(model.player, _.add(i))))

    case Cmds.UseConsumable(h: Consumables.HealthPotion.type) =>
      val possibleAmount  = model.player.fighter.maxHp - model.player.fighter.hp
      val amountRecovered = Math.min(possibleAmount, h.amount)

      if amountRecovered <= 0 then
        Outcome(
          model.next(inventoryLens.modify(model.player, _.add(h))),
          Batch(
            GameEvent.Log(
              Message("Your health is already full.", ColorScheme.impossible)
            )
          )
        )
      else
        Outcome(
          model.next(model.player.heal(amountRecovered)),
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
      handleHostileEvent(context, model.player)(e).map(model.next)

    case Cmds.Update =>
      Outcome(model)

  def nextViewModel(
      context: SceneContext[Size],
      model: PlayerM,
      viewModel: PlayerVM
  ): Cmds => Outcome[PlayerVM] =
    case Cmds.Update =>
      model.gameState match
        case GameState.UpdatingPlayer =>
          for {
            pp <- viewModel.playerPosition
              .next(
                context.delta,
                model.player.position,
                GameEvent.PlayerMoveComplete
              )
          } yield viewModel.copy(
            playerPosition = pp,
            sprite = viewModel.sprite
          )

        case _ =>
          Outcome(viewModel)

    case _ =>
      Outcome(viewModel)

  def view(
      context: SceneContext[Size],
      model: PlayerM,
      viewModel: PlayerVM
  ): Outcome[Batch[SceneNode]] =
    val radius =
      ((viewModel.squareSize.x / 2.5d) * (viewModel.playerPosition.attacking + 1.0)).toInt

    val playerCycle: CycleLabel =
      viewModel.playerPosition.state match
        case ActorMoveState.Idle           => PlayerVM.IdleCycle
        case ActorMoveState.MovingUp       => PlayerVM.WalkCycle
        case ActorMoveState.MovingDown     => PlayerVM.WalkCycle
        case ActorMoveState.MovingLeft     => PlayerVM.WalkCycle
        case ActorMoveState.MovingRight    => PlayerVM.WalkCycle
        case ActorMoveState.AttackingUp    => PlayerVM.AttackCycle
        case ActorMoveState.AttackingDown  => PlayerVM.AttackCycle
        case ActorMoveState.AttackingLeft  => PlayerVM.AttackCycle
        case ActorMoveState.AttackingRight => PlayerVM.AttackCycle

    val player: Outcome[Sprite[Bitmap]] =
      viewModel.sprite match // TODO: Can we not do this match every frame?!?
        case Some(plr) =>
          val p =
            plr
              .moveTo(viewModel.playerPosition.moving(viewModel.squareSize))
              .changeCycle(playerCycle)
              .play()

          // TODO: This is poor, we need the state to be stored between frames so that we can stay flipped.
          viewModel.playerPosition.state match
            case ActorMoveState.Idle           => Outcome(p)
            case ActorMoveState.MovingUp       => Outcome(p)
            case ActorMoveState.MovingDown     => Outcome(p)
            case ActorMoveState.MovingLeft     => Outcome(p.flipHorizontal(true))
            case ActorMoveState.MovingRight    => Outcome(p)
            case ActorMoveState.AttackingUp    => Outcome(p)
            case ActorMoveState.AttackingDown  => Outcome(p)
            case ActorMoveState.AttackingLeft  => Outcome(p.flipHorizontal(true))
            case ActorMoveState.AttackingRight => Outcome(p)

        case None =>
          Outcome.raiseError(new Exception("Player sprite missing."))

    player.map { plr =>
      Batch(
        Shape.Circle(
          viewModel.playerPosition.moving(viewModel.squareSize),
          radius,
          Fill.Color(RGBA.Black.withAlpha(0.5)),
          Stroke.None
        ),
        plr
      )
    }

  def handleHostileEvent(
      context: SceneContext[Size],
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

  final case class PlayerM(player: Player, gameState: GameState):
    def next(newPlayer: Player): PlayerM =
      this.copy(player = newPlayer)

  final case class PlayerVM(
      playerPosition: ActorPosition,
      squareSize: Point,
      sprite: Option[Sprite[Bitmap]]
  )
  object PlayerVM:
    val PlayerCycle: CycleLabel = CycleLabel("Player")
    val IdleCycle: CycleLabel   = CycleLabel("Idle")
    val WalkCycle: CycleLabel   = CycleLabel("walk")
    val AttackCycle: CycleLabel = CycleLabel("attack")
    val HitCycle: CycleLabel    = CycleLabel("hit")

  enum Cmds:
    case Update
    case RemoveFromInvetory(at: Int)
    case GiveToPlayer(item: Item)
    case UseConsumable(c: Consumables)
    case HostileInteraction(e: HostileEvent)
